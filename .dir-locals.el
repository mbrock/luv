((nil . ((eval .
          (let* ((root (expand-file-name
                        (locate-dominating-file default-directory "flake.nix")))
                 (bootstrap (expand-file-name "sly-init.lisp" root)))
            (setq-local
             sly-lisp-implementations
             `((luv ("nix" "develop" ,root
                     "--command" "sbcl"
                     "--dynamic-space-size" "4096"
                     "--noinform"
                     "--load" ,bootstrap))))
            (setq-local sly-default-lisp 'luv))))))
