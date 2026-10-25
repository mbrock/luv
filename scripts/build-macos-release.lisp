;;;; Called by build-macos-release in the pinned Nix development environment.
(require :asdf)
(asdf:load-asd (merge-pathnames "slynk.asd"
                              (uiop:ensure-directory-pathname
                               (uiop:getenv "LUV_SLYNK_DIR"))))
(asdf:load-system "luvcraft/program")
(asdf:load-system "deploy")

(defpackage #:luvcraft.macos-release (:use #:cl))
(in-package #:luvcraft.macos-release)

(defvar *resources*
  (uiop:ensure-directory-pathname (uiop:getenv "LUV_RELEASE_RESOURCES")))

;; Force lazy game libraries into Deploy's inventory before dumping. Their
;; handles survive as CFFI objects and Deploy reopens them at the new location.
(luv.ghostty:load-libghostty-vt)
(luv.libav:load-libav)

;; Apple's libraries live in the dyld shared cache, not necessarily on disk.
;; Keep their original specs; only non-system libraries belong in the app.
(defun system-library-spec-p (spec)
  (typecase spec
    ((or string pathname)
     (let ((path (namestring (pathname spec))))
       (cffi:foreign-funcall "_dyld_shared_cache_contains_path" :string
                            (if (find #\/ path) path
                                (concatenate 'string "/usr/lib/" path))
                            :boolean)))
    (cons (case (first spec)
            (:framework
             (system-library-spec-p
              (format nil "/System/Library/Frameworks/~A.framework/~A"
                      (second spec) (second spec))))
            (:or (some #'system-library-spec-p (rest spec)))))))

(dolist (library (deploy:list-libraries))
  ;; Definitions for unused tools (for example Tracy) are not game runtime
  ;; dependencies. The SBCL runtime's compression library is found by Mach-O
  ;; traversal, even though Deploy also defines an unopened CFFI entry for it.
  ;; A resolved on-disk library takes precedence over system fallback specs
  ;; (notably Nix libffi versus /usr/lib/libffi.dylib).
  (when (or (not (deploy:library-open-p library))
            (if (deploy:library-path library)
                (system-library-spec-p (deploy:library-path library))
                (system-library-spec-p (cffi::foreign-library-spec library))))
    (setf (deploy:library-dont-deploy-p library) t))
  (format t "Native library ~A: ~A (~S)~%"
          (deploy:library-name library) (deploy:library-path library)
          (cffi::foreign-library-spec library)))

;; Font paths are ASDF components in cl-dejavu and cached pathnames in McCLIM.
;; Keep this adjustment in the delivered image, not in development builds.
(uiop:run-program
 (list "/bin/cp" "-R"
       (namestring (asdf:system-relative-pathname
                    "cl-dejavu" "dejavu-fonts-ttf-2.37"))
       (namestring (merge-pathnames "fonts/dejavu" *resources*))))
(defun bundled-dejavu-font (name)
  (merge-pathnames name (merge-pathnames "fonts/dejavu/ttf/" *resources*)))
(setf (fdefinition 'cl-dejavu:font-pathname) #'bundled-dejavu-font)

(defvar *companion-mesh-files*
  (loop for kind in '(:cat :gnome :fantus)
        for source = (luvcraft.agent::companion-mesh-path kind)
        for destination = (merge-pathnames
                           (file-namestring source)
                           (merge-pathnames "companions/" *resources*))
        do (ensure-directories-exist destination)
           (uiop:copy-file source destination)
           (uiop:copy-file (make-pathname :type "rig" :defaults source)
                          (make-pathname :type "rig" :defaults destination))
        collect (cons kind (file-namestring source))))

(defun bundled-companion-mesh (kind)
  (merge-pathnames (or (cdr (assoc kind *companion-mesh-files*))
                       (error "Unknown companion ~S" kind))
                   (merge-pathnames "companions/" *resources*)))
(setf (fdefinition 'luvcraft.agent::companion-mesh-path) #'bundled-companion-mesh)

(deploy:define-hook (:boot luvcraft-resources 100) ()
  (setf *resources* (truename (merge-pathnames "../Resources/"
                                             (luvcraft::executable-directory)))
        mcluv::*bundled-fonts-directory* (merge-pathnames "fonts/" *resources*)
        mcluv::*gpu-sans-serif-fonts*
        (cons (bundled-dejavu-font "DejaVuSans.ttf")
              (bundled-dejavu-font "DejaVuSans-Bold.ttf"))
        luft.render::*terminal-font-pathname*
        (merge-pathnames "fonts/MonaspaceNeon-Regular.ttf" *resources*)
        luft.render::*terminal-bold-font-pathname*
        (merge-pathnames "fonts/MonaspaceNeon-Bold.ttf" *resources*)
        luv.libav::*libav-default-directory* nil
        luv.ghostty::*libghostty-vt-default-path* nil)
  (dolist (entry mcclim-truetype:*families/faces*)
    (setf (cdr entry)
          (bundled-dejavu-font (file-namestring (cdr entry)))))
  (mcluv::adopt-user-sans-serif-fonts))

;; Deploy's data directory is relative to the runtime, not the separate core.
(deploy:define-hook (:build resource-location -100) ()
  (setf deploy:*data-location* #P"../Resources/"))

;; The manifest lets the Mach-O packager recurse from the exact libraries
;; selected by CFFI, including both generated Objective-C helper dylibs.
(with-open-file (out (merge-pathnames "native-libraries.txt" *resources*)
                     :direction :output :if-exists :supersede)
  (dolist (library (deploy:list-libraries))
    (unless (deploy:library-dont-deploy-p library)
      (let ((path (deploy:library-path library)))
        (unless path (error "No file for ~A" library))
        (format out "~A~%" path)))))
(uiop:copy-file sb-ext:*runtime-pathname*
                (merge-pathnames "../MacOS/luvcraft-runtime" *resources*))

;; A separate core permits install_name_tool and codesign on the runtime.
;; Signing an executable with an appended SBCL core can destroy its trailer.
(let ((operation (asdf:make-operation 'deploy:deploy-op)))
  (setf (deploy:output-file operation) (merge-pathnames "luvcraft.core" *resources*)
        (deploy:entry-point operation) #'luvcraft:main)
  (deploy:deploy operation :type :image))
