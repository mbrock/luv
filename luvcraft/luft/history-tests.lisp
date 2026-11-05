;;;; History across saves (#OVEP9A, #4D3QFJ): the journal's tail, per-player
;;;; undo after a reload, and who authored each cell.  These use the session
;;;; and players-table tests' worlds and pumps.
(in-package #:luvcraft.luft.game.tests)

(defun journal-head (world)
  (terrain:world-journal-head (terrain:world-source-journal (host:world-source world))))

(defun shown-at (world cell)
  (terrain:scene-material-placement-at (host:world-scene world) cell))

(define-test bea-undoes-her-own-edit-after-a-reload-leaving-adas
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let ((stone (game:block-placement luvcraft::*stone-block*))
          (planks (game:block-placement luvcraft::*planks-block*))
          (hers nil) (adas nil) (head nil))
      (with-host (world session network)
        (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
          (let ((ada (host:find-world-player world "ada")))
            (setf hers (air-cell world 1033 1044)
                  adas (air-cell world 1035 1044))
            (is eq :requested (nth-value 1 (host:player-edit-world-cell guest-world bea hers stone)))
            (true (pump (list session guest) :until (applied-p guest world)))
            ;; Ada builds after her, so Bea's edit is not the journal's last.
            (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada adas planks)))
            (true (pump (list session guest) :until (applied-p guest world)))
            (setf head (journal-head world))
            (host:close-world-session guest)
            (true (pump (list session) :until (lambda () (null (host:find-world-player world "bea")))))
            (host:save-luvcraft-world world ada pathname))))
      ;; The host comes back from its save, and Bea joins again and undoes.
      (let* ((network (luvcraft.wire.net:make-loopback-network))
             (world (world-from-save pathname))
             (session (host:host-luvcraft-world world (host:find-world-player world "ada")
                                                :loopback network :name "host")))
        (unwind-protect
             (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
               (declare (ignore bea))
               ;; Numbering goes on where it stopped.
               (is = head (journal-head world))
               (is eq stone (shown-at world hers))
               (host:session-undo guest)
               (true (pump (list session guest)
                           :until (lambda () (and (> (journal-head world) head)
                                                  (funcall (applied-p guest world))))))
               (is eq nil (shown-at world hers))
               (is eq nil (shown-at guest-world hers))
               (is eq planks (shown-at world adas))
               (is equal "bea" (last-actor world))
               ;; Ada's undo, across the same reload, takes back only hers.
               (is eq :edited (nth-value 1 (terrain:undo-world-events (host:world-scene world) "ada")))
               (is eq nil (shown-at world adas))
               (host:close-world-session guest))
          (host:close-world-session session))))))

(define-test a-companions-edit-keeps-its-author-across-a-reload
  ;; Once in the saved tail, and once compacted into the edit set.
  (dolist (tail (list terrain:*journal-tail-events* 0))
    (uiop:with-temporary-file (:pathname pathname :type "sexp")
      (let* ((world (session-test-world))
             (gnome (summon-test-companion world 1040 1040))
             (cell (air-cell world 1041 1042 0))
             (stone (game:block-placement luvcraft::*stone-block*)))
        (is eq :edited (nth-value 1 (host::edit-agent-cell world gnome cell stone)))
        (let ((made (host::world-cell-provenance world cell)))
          (let ((terrain:*journal-tail-events* tail))
            (host:save-luvcraft-world world (host:find-world-player world "ada") pathname))
          (multiple-value-bind (block-world resume inhabitants journal)
              (luvcraft:read-luvcraft-save pathname)
            (declare (ignore block-world resume inhabitants))
            (is = (if (zerop tail) 0 1) (length (getf journal :journal))))
          (let ((again (world-from-save pathname)))
            (host::initialize-world-agents again)
            (let ((kept (host::world-cell-provenance again cell)))
              (true kept)
              (is equal (list :inhabitant (host::inhabitant-id gnome))
                  (terrain:cell-provenance-actor kept))
              (is = (terrain:cell-provenance-sequence made) (terrain:cell-provenance-sequence kept))
              (is eql (terrain:cell-provenance-time made) (terrain:cell-provenance-time kept)))
            (is equal (format nil "placed by the gnome, today at ~A"
                              (multiple-value-bind (s minute hour)
                                  (decode-universal-time
                                   (terrain:cell-provenance-time made))
                                (declare (ignore s))
                                (format nil "~2,'0D:~2,'0D" hour minute)))
                (host::cell-history-phrase again cell))))))))

(define-test a-save-keeps-the-newest-transactions-and-undo-reaches-only-them
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let* ((world (session-test-world))
           (ada (host:find-world-player world "ada"))
           (stone (game:block-placement luvcraft::*stone-block*))
           (cells (loop for x from 1030 below 1035 collect (air-cell world x 1050))))
      (dolist (cell cells)
        (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada cell stone))))
      ;; Two events fit in the tail: the first three are compacted.
      (let ((terrain:*journal-tail-events* 2))
        (host:save-luvcraft-world world ada pathname))
      (multiple-value-bind (block-world resume inhabitants journal)
          (luvcraft:read-luvcraft-save pathname)
        (declare (ignore block-world resume inhabitants))
        (is = 3 (getf journal :watermark))
        (is equal '(4 5) (mapcar (lambda (event) (getf event :sequence)) (getf journal :journal))))
      (let* ((again (world-from-save pathname))
             (scene (host:world-scene again)))
        (is = 5 (journal-head again))
        (dolist (cell cells)
          (is eq stone (shown-at again cell)))
        ;; A compacted cell still knows who placed it.
        (is equal "ada" (terrain:cell-provenance-actor
                         (host::world-cell-provenance again (first cells))))
        (is = 1 (terrain:cell-provenance-sequence
                 (host::world-cell-provenance again (first cells))))
        ;; Undo reaches into the tail, and no further.
        (is eq :edited (nth-value 1 (terrain:undo-world-events scene "ada")))
        (is eq :edited (nth-value 1 (terrain:undo-world-events scene "ada")))
        (is eq :nothing (nth-value 1 (terrain:undo-world-events scene "ada")))
        (is equal (list stone stone stone nil nil)
            (mapcar (lambda (cell) (shown-at again cell)) cells))
        (is = 7 (journal-head again))))))
