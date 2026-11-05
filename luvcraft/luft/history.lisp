;;;; Fine history in the world (#OVEP9A): who authored a cell and when, read
;;;; from the journal's provenance.
(in-package #:luvcraft.luft.game)

(defun world-cell-provenance (world cell)
  "Who authored CELL's current value in WORLD, and when: a
TERRAIN:CELL-PROVENANCE, or NIL when CELL is not authored or its author is
unknown, as an edit made before provenance was kept."
  (terrain:world-cell-provenance (terrain:world-source-journal (world-source world)) cell))

(defun actor-name (world actor)
  "What to call journal ACTOR in WORLD: a player's name, a companion's kind."
  (cond ((stringp actor)
         (let ((player (find-world-player world actor))
               (record (absent-player-record world actor)))
           (or (and player (player-name player))
               (getf record :name)
               actor)))
        ((and (consp actor) (eq (first actor) :inhabitant))
         (let ((inhabitant (find-world-inhabitant world (second actor))))
           (if inhabitant
               (format nil "the ~(~A~)" (inhabitant-kind inhabitant))
               "a companion")))
        ((eq actor :import) "an import")
        ((eq actor :local) "you")
        (t (princ-to-string actor))))

(defparameter *weekday-names*
  #("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defparameter *month-names*
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun when-phrase (time &optional (now (get-universal-time)))
  "When universal TIME was, as a person says it from NOW: \"today at 14:05\",
\"yesterday\", \"Tuesday\" within the week, else \"3 November\"."
  (multiple-value-bind (s minute hour day month year weekday) (decode-universal-time time)
    (declare (ignore s))
    (multiple-value-bind (s m h today-day today-month today-year) (decode-universal-time now)
      (declare (ignore s m h))
      (let ((days (- (floor (encode-universal-time 0 0 12 today-day today-month today-year) 86400)
                     (floor (encode-universal-time 0 0 12 day month year) 86400))))
        (cond ((<= days 0) (format nil "today at ~2,'0D:~2,'0D" hour minute))
              ((= days 1) "yesterday")
              ((< days 7) (aref *weekday-names* weekday))
              ((= year today-year) (format nil "~D ~A" day (aref *month-names* (1- month))))
              (t (format nil "~D ~A ~D" day (aref *month-names* (1- month)) year)))))))

(defun cell-history-phrase (world cell &key (now (get-universal-time)))
  "How the inspector says who authored CELL and when, such as \"placed by
Bea, Tuesday\", or NIL when nobody is known to have."
  (let ((provenance (world-cell-provenance world cell)))
    (when provenance
      (let ((time (terrain:cell-provenance-time provenance))
            (tick (terrain:cell-provenance-tick provenance)))
        (format nil "~:[removed~;placed~] by ~A~@[, ~A~]"
                (world-authored-block world cell)
                (actor-name world (terrain:cell-provenance-actor provenance))
                (cond (time (when-phrase time now))
                      (tick (format nil "at tick ~D" tick))))))))
