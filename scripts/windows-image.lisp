;;;; Windows-only delivery adaptations. They live in the dumped image, not
;;;; in the development system, and need neither ASDF sources nor MSYS2 at boot.
(asdf:load-system "deploy")
(asdf:load-system "luvcraft/program")

(when (find-package "SLYNK")
  (error "The Windows game must not load the Slynk development endpoint."))

(defun windows-resource (relative)
  (merge-pathnames relative (deploy:data-directory)))

;; Copy resources before replacing the two functions that consult ASDF at
;; runtime. Deploy clears the ASDF registry before saving its image.
(let ((output (windows-build-directory "LUV_WINDOWS_OUTPUT")))
  (deploy:copy-directory-tree
   (asdf:system-relative-pathname "luv" "fonts/") output)
  (deploy:copy-directory-tree
   (asdf:system-relative-pathname "cl-dejavu" "dejavu-fonts-ttf-2.37/")
   (merge-pathnames "dejavu/" output) :copy-root nil)
  (dolist (kind '(:cat :gnome :fantus))
    (let* ((source (luvcraft.agent::companion-mesh-path kind))
           (target (merge-pathnames
                    (format nil "companions/~(~A~).fmesh" kind) output)))
      (ensure-directories-exist target)
      (uiop:copy-file source target)
      (uiop:copy-file (make-pathname :type "rig" :defaults source)
                      (make-pathname :type "rig" :defaults target)))))

(defun cl-dejavu:font-pathname (name)
  (windows-resource (format nil "dejavu/ttf/~A" name)))

(defun luvcraft.agent::companion-mesh-path (kind)
  (check-type kind (member :cat :gnome :fantus))
  (windows-resource (format nil "companions/~(~A~).fmesh" kind)))

(deploy:define-hook (:boot windows-resources most-positive-fixnum) ()
  (setf mcluv::*bundled-fonts-directory* (windows-resource "fonts/")
        mcluv::*gpu-sans-serif-fonts*
        (cons (cl-dejavu:font-pathname "DejaVuSans.ttf")
              (cl-dejavu:font-pathname "DejaVuSans-Bold.ttf"))
        luv.libav::*libav-default-directory* (namestring (deploy:data-directory)))
  ;; McCLIM's raster styles captured DejaVu pathnames while loading too.
  (dolist (entry mcclim-truetype:*families/faces*)
    (let ((name (file-namestring (cdr entry))))
      (setf (cdr entry)
            (or (probe-file (merge-pathnames name mcluv::*bundled-fonts-directory*))
                (cl-dejavu:font-pathname name)))))
  (mcluv::adopt-user-sans-serif-fonts)
  ;; The Windows build excludes Luft's terminal overlay, but Luvcraft's
  ;; shared world-text definitions may still retain the bundled font paths.
  (setf luvcraft::*luvcraft-fonts-directory* (windows-resource "fonts/")
        luvcraft::*terminal-display-font-pathname*
        (windows-resource "fonts/MonaspaceNeon-Regular.ttf")
        luvcraft::*terminal-display-bold-font-pathname*
        (windows-resource "fonts/MonaspaceNeon-Bold.ttf"))
  (dolist (file (append
                (mapcar #'cdr mcclim-truetype:*families/faces*)
                (loop for kind in '(:cat :gnome :fantus)
                      for mesh = (luvcraft.agent::companion-mesh-path kind)
                      append (list mesh (make-pathname :type "rig" :defaults mesh)))))
    (unless (probe-file file)
      (error "Missing bundled Windows game resource: ~A" file))))

;; Registered but unused bindings (Ghostty, Tracy, etc.) need no DLL. Lazy
;; media/Vulkan/SSL libraries keep their Windows basename specs; Python adds
;; those DLLs and their recursive PE import closure after the image is saved.
(dolist (library (deploy:list-libraries))
  (unless (deploy:library-open-p library)
    (setf (deploy:library-dont-deploy-p library) t)))

;; Keep redirected stdout/stderr intact for CI/--help instead of Deploy's
;; AttachConsole/UTF-16 conversion. Python changes just the PE subsystem to
;; GUI after dumping, preserving the appended SBCL core and avoiding a console
;; window on double-click. Deploy still closes/reopens CFFI libraries.
(let ((operation (asdf:make-operation 'deploy:deploy-console-op)))
  (setf (deploy:output-file operation)
        (merge-pathnames "luvcraft.exe"
                         (windows-build-directory "LUV_WINDOWS_OUTPUT"))
        (deploy:entry-point operation) #'luvcraft:main)
  (deploy:deploy operation))
