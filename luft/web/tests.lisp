(defpackage #:luft.web.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true)
  (:local-nicknames (#:web #:luft.web)))

(in-package #:luft.web.tests)

(defun occupancy-fixtures ()
  "Every 2x2x2 occupancy, translated off the origin into negative coordinates."
  (loop for mask below 256
        for cells = (make-hash-table :test #'equal)
        do (dotimes (sample 8)
             (when (logbitp sample mask)
               (setf (gethash (list (- (ldb (byte 1 0) sample) 3)
                                   (+ (ldb (byte 1 1) sample) 5)
                                   (- (ldb (byte 1 2) sample) 2))
                              cells) 1)))
        collect
        (list (loop for cell being the hash-keys of cells
                    collect (append cell '(1)))
              (loop for site being the hash-keys of (luft:star-surface-sites cells)
                    using (hash-value star)
                    collect (append site (list star))))))

(defun browser-claims ()
  (parenscript:ps*
   `(progn
      (defvar luft.web::initial-cells (parenscript:array))
      ,(web::core-form)
      (defvar fixtures ,(web::array-form (occupancy-fixtures)))
      ,(let ((*package* (find-package '#:luft.web)))
         (read-from-string
          "(progn
            (defun claim (value message)
              (unless value (throw (new (|Error| message)))))
            ((@ fixtures for-each)
             (lambda (fixture)
               (setf initial-cells (aref fixture 0))
               (reset-cells)
               (let ((sites (surface-sites)) (expected (aref fixture 1)))
                 (claim (= (@ sites size) (@ expected length)) \"site count\")
                 ((@ expected for-each)
                  (lambda (site)
                    (let ((actual ((@ sites get) (cell-key (aref site 0) (aref site 1) (aref site 2)))))
                      (claim (and actual (= (aref actual 3) (aref site 3))) \"native star parity\")))))))
            ((@ cells clear))
            ((@ cells set) (cell-key 2 0 0) 2)
            (let ((hit (trace-cells (array 0.5 0.5 0.5) (array 1 0 0) 4)))
              (claim (= (aref (@ hit cell) 0) 2) \"axis-aligned hit\")
              (claim (= (aref (@ hit previous) 0) 1) \"placement neighbor\"))
            (claim (= (trace-cells (array 0.5 0.5 0.5) (array -1 0 0) 4) null) \"miss\")
            (claim (= (trace-cells (array 0.5 0.5 0.5) (array 1 0 0) 1) null) \"reach\")
            (claim (collides 2.5 0.5 0) \"body intersects solid\")
            (claim (not (collides 2.5 0.5 1)) \"standing above solid\")
            (claim (not (collides 0.5 0.5 0)) \"empty body\")
            ((@ console log) \"256 native occupancy fixtures and browser picking/collision passed\"))")))))

(define-test browser-star-selection-matches-native
  ;; Execute the actual compiled browser code, not a second Lisp port of it.
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (browser-claims) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" (namestring path))
                          :output :string :error-output :string
                          :ignore-error-status t)
      (unless (zerop code) (error "Browser claims failed:~%~A" errors))
      (true (search "passed" output)))))

(define-test demo-publishes-without-a-native-gpu
  (let ((resources (web:demo-resources nil)))
    (true (= 3 (length resources)))
    (true (search "importmap" (web::demo-html)))
    (true (search "luft-demo.js" (web::demo-html)))
    (true (> (length (web::demo-cells)) 10000))))

(define-test complete-demo-is-valid-javascript-module
  (uiop:with-temporary-file (:pathname path :stream stream :type "mjs")
    (write-string (web:demo-javascript) stream)
    (finish-output stream)
    (multiple-value-bind (output errors code)
        (uiop:run-program (list "node" "--check" (namestring path))
                          :output :string :error-output :string
                          :ignore-error-status t)
      (declare (ignore output))
      (unless (zerop code) (error "Demo compilation failed:~%~A" errors))
      (true (zerop code)))))
