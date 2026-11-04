(in-package #:luft.terrain)

;;; Peers agree on a world by hash, chunk by chunk (#RELI34).  A world tag
;;; names what a source generates: its recipe, which a game supplies with its
;;; generator version first, and a digest of its material vocabulary.  Edits
;;; are not in the tag; they travel as events.  A chunk hash names what one
;;; resident chunk holds after its edits: occupancy, materials, and the
;;; attachments its fixtures became.
;;;
;;; Both are MD5 over canonical octets, in hexadecimal.  MD5 is a checksum
;;; here, not a defence: peers compare honest worlds.  Every collection is
;;; written in a sorted order, never in hash-table order, so a hash depends
;;; on neither the thread nor the insertion history that built the value.

(defgeneric world-source-recipe (source)
  (:documentation
   "SOURCE's deterministic description, without its edits: a tree of
integers, strings, and symbols whose printed form decides everything SOURCE
generates.  It names its generator and that generator's version first, so a
generator change that alters any chunk changes every tag."))

(defun canonical-recipe-text (recipe)
  "RECIPE printed under standard syntax, after checking it holds no floats."
  (labels ((check (form)
             (typecase form
               (cons (check (car form)) (check (cdr form)))
               ((or integer string symbol) form)
               (t (error "A world recipe holds only integers, strings, and ~
                          symbols, not ~S." form)))))
    (check recipe))
  (with-standard-io-syntax
    (let ((*print-readably* t))
      (prin1-to-string recipe))))

(defun octets-hex (octets)
  (format nil "~(~{~2,'0X~}~)" (coerce octets 'list)))

(defun text-octets (string)
  (sb-ext:string-to-octets string :external-format :utf-8))

(defun material-vocabulary-digest (vocabulary)
  "A digest of VOCABULARY's members in offset order: each placement's name,
its kind's name, and the kind's attachment form, which decides whether its
cells stay cubes.  Tones and light are appearance, not agreement."
  (octets-hex
   (sb-md5:md5sum-sequence
    (text-octets
     (canonical-recipe-text
      (cons :luft-material-vocabulary
            (loop for member across (domains:identity-vocabulary-members vocabulary)
                  collect (and member
                               (let ((kind (material-placement-kind member)))
                                 (list (material-placement-name member)
                                       (material-kind-name kind)
                                       (material-kind-attachment-form kind)))))))))))

(defun world-source-tag (source)
  "SOURCE's world tag: its recipe and its vocabulary's digest, hashed."
  (octets-hex
   (sb-md5:md5sum-sequence
    (text-octets
     (canonical-recipe-text
      (list :luft-world-tag 1
            (world-source-recipe source)
            (material-vocabulary-digest
             (world-source-material-vocabulary source))))))))

(defun resident-cell-chunk-hash (resident)
  "The content hash of RESIDENT: its key, its fiber words, its material cells
by ascending site, and its attachments by ascending clearance cell.

Material offsets are written as they are, so a hash is meaningful between
peers whose vocabularies share a digest."
  (let ((state (sb-md5:make-md5-state))
        (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
        (fill 0))
    (labels ((flush ()
               (sb-md5:update-md5-state state buffer :end fill)
               (setf fill 0))
             (word (value)
               (when (> (+ fill 8) (length buffer)) (flush))
               (dotimes (i 8)
                 (setf (aref buffer fill) (ldb (byte 8 (* 8 i)) value))
                 (incf fill)))
             (text (string)
               (let ((octets (text-octets string)))
                 (word (length octets))
                 (flush)
                 (sb-md5:update-md5-state state octets))))
      (text "luft resident chunk 1")
      (word (resident-cell-chunk-key resident))
      (loop for value across (luft:chunk-fibers-words
                              (resident-cell-chunk-fibers resident))
            do (word value))
      (let* ((materials (resident-cell-chunk-material-cells resident))
             (cells (make-array (hash-table-count materials)
                                :element-type '(unsigned-byte 64)))
             (index 0))
        (maphash (lambda (cell offset)
                   (declare (ignore offset))
                   (setf (aref cells index) cell)
                   (incf index))
                 materials)
        (word (length cells))
        (loop for cell across (sort cells #'<)
              do (word cell)
                 (word (gethash cell materials))))
      ;; A live edit appends its attachment; a fresh materialization sorts
      ;; them by face.  Both describe one set.
      (let ((attachments (sort (copy-list (resident-cell-chunk-attachments resident))
                               #'< :key #'surface-attachment-clearance-cell)))
        (word (length attachments))
        (dolist (attachment attachments)
          (word (surface-attachment-support-cell attachment))
          (word (surface-attachment-face attachment))
          (word (surface-attachment-clearance-cell attachment))
          (text (canonical-recipe-text
                 (material-kind-name (surface-attachment-material attachment))))))
      (flush)
      (octets-hex (sb-md5:finalize-md5-state state)))))

(defun world-source-chunk-hash (source key)
  "The content hash of chunk KEY materialized from SOURCE and its edits, and
the materialized resident chunk."
  (let ((resident (materialize-world-source-chunk
                   source key 1
                   :edits (capture-world-source-chunk-edits source key))))
    (values (resident-cell-chunk-hash resident) resident)))

(defun streaming-scene-chunk-hash (scene key)
  "The content hash of SCENE's resident chunk KEY after its edits, or NIL
while KEY is not resident."
  (let ((resident (gethash key (streaming-scene-store scene))))
    (and (typep resident 'resident-cell-chunk)
         (resident-cell-chunk-hash resident))))
