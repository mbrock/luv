((nil . ((eval .
          (let* ((root (expand-file-name
                        (locate-dominating-file default-directory "flake.nix")))
                 (bootstrap (expand-file-name "sly-init.lisp" root))
                 (dev (expand-file-name "scripts/dev" root))
                 (port-script (expand-file-name "scripts/worktree-port" root))
                 (port-buffer (generate-new-buffer " *luv worktree port*"))
                 port-status
                 port)
            (unwind-protect
                (progn
                  (setq port-status
                        (call-process port-script nil port-buffer nil))
                  (with-current-buffer port-buffer
                    (setq port (string-trim (buffer-string)))))
              (kill-buffer port-buffer))
            (unless (and (integerp port-status)
                         (zerop port-status)
                         (string-match-p "\\`[0-9]+\\'" port))
              (error "Could not derive luv worktree Slynk port (status %S): %s"
                     port-status port))
            (setq-local
             sly-lisp-implementations
             `((luv ("env" ,(format "LUV_SLYNK_PORT=%s" port)
                     ,dev "sbcl"
                     "--dynamic-space-size" "6144"
                     "--noinform"
                     "--eval" "(proclaim '(optimize (debug 3)))"
                     "--load" ,bootstrap))))
            (setq-local sly-default-lisp 'luv))))))
