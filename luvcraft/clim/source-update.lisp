(in-package #:luvcraft.clim)

(define-command (com-review-source-update
                 :command-table luvcraft-world
                 :name "Review Source Update")
    ()
  (mcluv:open-luvcraft-source-update (luvcraft-command-session)))
