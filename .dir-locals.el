((nil . ((eval .
          (let* ((root (expand-file-name
                        (locate-dominating-file default-directory "flake.nix")))
                 (bootstrap (expand-file-name "sly-init.lisp" root)))
            (setq-local
             sly-lisp-implementations
             `((luv ("nix" "develop" ,root
                     "--command" "sbcl"
                     "--dynamic-space-size" "6144"
                     "--noinform"
                     "--eval" "(proclaim '(optimize (debug 3)))"
                     "--load" ,bootstrap))))
            (setq-local sly-default-lisp 'luv))))))
