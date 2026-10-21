(in-package #:luvcraft.agent.tests)

;;; The introspection tools answer from the image the tests run in, so these
;;; ask about definitions that exist in this checkout and check that the
;;; answers carry what an agent needs to act on them: a file and line.

(define-test describe-symbol-names-kind-lambda-list-and-source
  (let ((answer (agent:com-describe-symbol "block-kind-named")))
    (true (search "luvcraft:block-kind-named: function" answer))
    (true (search "Lambda list: (name &optional (error-p t))" answer))
    (true (search "Defined: luvcraft/blocks.lisp:" answer)))
  (true (search "Slots:" (agent:com-describe-symbol "luvcraft:block-kind")))
  (true (search "Nothing defined" (agent:com-describe-symbol "no-such-thing-at-all"))))

(define-test apropos-finds-defined-names-in-the-game
  (let ((answer (agent:com-apropos "quickbar-slot")))
    (true (search "luvcraft:assign-luvcraft-quickbar-slot (function)" answer))
    (false (search "sb-" answer))))

(define-test source-reads-the-whole-definition-from-its-file
  (let ((answer (agent:com-source "assign-luvcraft-quickbar-slot")))
    (true (search "luvcraft/app.lisp:" answer))
    (true (search "(defun assign-luvcraft-quickbar-slot (session block slot)"
                  answer))
    (true (search "(select-luvcraft-block session slot))))" answer))))

(define-test xref-answers-callers-with-places
  (let ((answer (agent:com-xref "callers" "luvcraft:select-luvcraft-block")))
    (true (search "Callers of luvcraft:select-luvcraft-block:" answer))
    (true (search "luvcraft/app.lisp:" answer)))
  (true (search "Unknown relation"
                (agent:com-xref "friends" "luvcraft:select-luvcraft-block"))))
