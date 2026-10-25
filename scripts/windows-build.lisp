;;;; Native SBCL preparation. Invoked by windows-build.py, never by Nix.
(require :asdf)

#-(and sbcl win32 x86-64)
(error "Build with native Windows x86-64 SBCL, not WSL or a Unix SBCL.")

;; McCLIM's Drei supports a game without the Swank development endpoint.
(pushnew :clim-without-swank *features*)

(defun windows-build-directory (variable)
  (uiop:ensure-directory-pathname
   (or (uiop:getenv variable) (error "Missing ~A." variable))))

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(windows-build-directory "LUV_WINDOWS_ROOT"))
   (:tree ,(windows-build-directory "LUV_WINDOWS_DEPS"))
   :ignore-inherited-configuration))

(asdf:initialize-output-translations
 `(:output-translations
   (,(windows-build-directory "LUV_WINDOWS_DEPS")
    (,(windows-build-directory "LUV_WINDOWS_CACHE") "deps/" :**/))
   (,(windows-build-directory "LUV_WINDOWS_ROOT")
    (,(windows-build-directory "LUV_WINDOWS_CACHE") "game/" :**/))
   :ignore-inherited-configuration))

(asdf:load-system "cffi-grovel")

;; SBCL's sbcl.mk can name the compiler from the MSYS2 package builder.
;; Do not inherit its paths or Unix flags. Grovel against THIS job's UCRT
;; headers and run its native helper executables before dumping the image.
(setf cffi-toolchain:*cc* (uiop:getenv "CC")
      cffi-toolchain:*ld* (uiop:getenv "CC")
      cffi-toolchain:*cc-flags* '("-m64")
      cffi-toolchain:*ld-exe-flags* '("-m64")
      cffi-toolchain:*ld-dll-flags* '("-m64" "-shared")
      cffi-toolchain:*linkkit-start* nil
      cffi-toolchain:*linkkit-end* nil)

(push (windows-build-directory "LUV_WINDOWS_DLLS")
      cffi:*foreign-library-directories*)

(load (merge-pathnames "scripts/windows-image.lisp"
                       (windows-build-directory "LUV_WINDOWS_ROOT")))
