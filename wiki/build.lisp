;;;; Build the ./wiki executable from a fresh SBCL in the luv development shell.

(require :asdf)

(let ((project-root
        (truename
         (merge-pathnames #P"../"
                          (uiop:pathname-directory-pathname *load-truename*)))))
  (asdf:load-asd (merge-pathnames #P"luv-wiki.asd" project-root)))

(asdf:load-system :luv-wiki/cli)

;;; Woo opens libev by leaf name.  That works here because nixpkgs' SBCL
;;; wrapper prefixes DYLD_LIBRARY_PATH, but the dumped program starts without
;;; the wrapper and reopens each shared object by its recorded name, which
;;; macOS then cannot find.  Record the file this build actually resolved.
(defun pin-leaf-shared-objects ()
  (let ((directories
          (loop for variable in '("DYLD_LIBRARY_PATH" "LD_LIBRARY_PATH")
                append (remove "" (uiop:split-string
                                   (or (uiop:getenv variable) "")
                                   :separator ":")
                               :test #'string=))))
    (dolist (object sb-sys:*shared-objects*)
      (let ((name (sb-alien::shared-object-namestring object)))
        (unless (find #\/ name)
          (let ((file (loop for directory in directories
                            thereis (probe-file
                                     (merge-pathnames
                                      name (uiop:ensure-directory-pathname
                                            directory))))))
            (when file
              (setf (sb-alien::shared-object-namestring object)
                    (namestring file)
                    (sb-alien::shared-object-pathname object)
                    file))))))))

(uiop:register-image-dump-hook 'pin-leaf-shared-objects)
(uiop:symbol-call '#:luv.wiki.cli '#:capture-asdf-configuration)
(asdf:make :luv-wiki/cli)
