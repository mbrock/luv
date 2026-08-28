;;; Luvcraft's verbs as CLIM commands.
;;;
;;; The game core knows nothing about McCLIM and the McCLIM layer knows only
;;; how to draw panes; this package is where the two meet.  It owns the
;;; application frame, the command tables that hold the game's named verbs and
;;; their keystrokes, and the translation from a portable luv key event into a
;;; command to execute.
;;;
;;; Commands run inline on the canvas thread, which is where every existing
;;; event handler already runs, so a command may touch session and world state
;;; exactly as a HANDLE-CANVAS-EVENT method may.  That is only true while the
;;; frame has no FRAME-PROCESS of its own: a frame with a running top level
;;; would execute commands on another thread and quietly break the
;;; single-writer rule the streaming system depends on.

(defpackage #:luvcraft.clim
  (:use #:clim-lisp #:clim)
  (:local-nicknames (#:luv #:luv)
                    (#:climi #:clim-internals))
  ;; SHADOWING-IMPORT also makes this package definition a live migration:
  ;; an image loaded before the bridge moved out of luvcraft may still own the
  ;; four old symbols when DEFPACKAGE is evaluated again.
  (:shadowing-import-from #:mcluv
                          #:canvas-key-event-matches-gesture-p
                          #:canvas-key-event-command
                          #:execute-canvas-key-event-command
                          #:format-gesture)
  (:export #:luvcraft-frame
           #:luvcraft-frame-session
           #:make-luvcraft-frame
           #:luvcraft-command-session
           #:canvas-key-event-matches-gesture-p
           #:canvas-key-event-command
           #:luvcraft-key-command
           #:luvcraft-key-event-tables
           #:luvcraft-focus-command-table
           #:execute-canvas-key-event-command
           #:com-toggle-inventory
           #:com-toggle-phone
           #:com-toggle-metabar
           #:com-toggle-lobby-panel
           #:com-toggle-fullscreen
           #:com-toggle-tracy-capture
           #:com-toggle-focus
           #:com-export-presentation-timing
           #:com-select-quickbar-slot
           #:com-leave-focus
           #:com-release-pointer
           #:com-start-walking
           #:com-stop-walking
           #:com-jump
           #:com-show-keymap
           #:com-execute-command
           #:com-review-source-update
           ;; The keymap legend, gathered from the tables rather than written.
           #:luvcraft-legend
           #:luvcraft-legend-overlay
           #:luvcraft-legend-sections
           #:open-luvcraft-legend
           #:close-luvcraft-legend
           #:toggle-luvcraft-legend
           ;; M-x: the searchable, executable command vocabulary.
           #:luvcraft-command-menu-overlay
           #:luvcraft-command-menu-entries
           #:open-luvcraft-command-menu
           #:close-luvcraft-command-menu
           #:toggle-luvcraft-command-menu))
