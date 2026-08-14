;;;; A minimal Parinfer-like indentation repair pass.
;;;;
;;;; Adapted from cl-mcp/src/parinfer.lisp:
;;;; https://github.com/cl-ai-project/cl-mcp
;;;;
;;;; Copyright 2025 cxxxr, Satoshi Imai
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a
;;;; copy of this software and associated documentation files (the "Software"),
;;;; to deal in the Software without restriction, including without limitation
;;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;;; and/or sell copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be included in
;;;; all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.

(defpackage #:sly-client/parinfer
  (:use #:cl)
  (:export #:apply-indent-mode
           #:analyze-indent-mode
           #:indent-mode-candidate
           #:indent-mode-report
           #:indent-mode-report-candidate
           #:indent-mode-report-candidate-balanced-p
           #:indent-mode-report-candidate-changed-p
           #:indent-mode-report-source-balanced-p))

(in-package #:sly-client/parinfer)

(defstruct (state (:constructor make-state ()))
  (stack nil :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (sharp-seen nil :type boolean)
  (char-literal nil :type boolean)
  (block-comment-depth 0 :type (integer 0))
  (block-sharp-seen nil :type boolean)
  (block-bar-seen nil :type boolean)
  (token-buffer nil :type list)
  token-form
  (unmatched-closes 0 :type (integer 0)))

(defstruct (open-form
            (:constructor make-open-form
                (column line &key open-column role head element-count)))
  (column 0 :type (integer 0))
  (line 0 :type (integer 0))
  (open-column 0 :type (integer 0))
  (role :ordinary :type symbol)
  head
  (element-count 0 :type (integer 0)))

(defstruct (close-event (:constructor make-close-event (open-form column)))
  open-form
  (column 0 :type (integer 0)))

(defstruct indent-mode-report
  (source-balanced-p nil :type boolean)
  (candidate "" :type string)
  (candidate-balanced-p nil :type boolean)
  (candidate-changed-p nil :type boolean))

(defparameter *binding-form-heads*
  '("LET" "LET*" "FLET" "LABELS" "MACROLET" "SYMBOL-MACROLET"))

(defparameter *clause-form-heads*
  '("COND" "CASE" "CCASE" "ECASE" "TYPECASE" "CTYPECASE" "ETYPECASE"))

(defparameter *slot-list-form-heads*
  '("DEFCLASS"))

(defun split-lines (text)
  (if (zerop (length text))
      nil
      (loop with start = 0
            for newline = (position #\Newline text :start start)
            collect (subseq text start newline)
            if newline
              do (setf start (1+ newline))
              and do (when (= start (length text)) (loop-finish))
            else
              do (loop-finish))))

(defun count-leading-space (line)
  (loop for character across line
        while (member character '(#\Space #\Tab))
        count 1))

(defun empty-or-comment-line-p (line)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
    (or (string= trimmed "")
        (char= (char trimmed 0) #\;))))

(defun blank-line-p (line)
  (string= (string-left-trim '(#\Space #\Tab) line) ""))

(defun leftmost-opening-line-p (line)
  (and (plusp (length line))
       (char= (char line 0) #\()))

(defun normalized-token (characters)
  (string-upcase (coerce (nreverse characters) 'string)))

(defun register-form-element (form)
  (when form
    (incf (open-form-element-count form))))

(defun finish-token (state)
  (when (state-token-buffer state)
    (let ((form (state-token-form state))
          (token (normalized-token (state-token-buffer state))))
      (when form
        (register-form-element form)
        (when (= (open-form-element-count form) 1)
          (setf (open-form-head form) token))))
    (setf (state-token-buffer state) nil
          (state-token-form state) nil)))

(defun token-character-p (character)
  (not (member character
               '(#\Space #\Tab #\Newline #\( #\) #\" #\;))))

(defun begin-or-continue-token (state character)
  (unless (state-token-buffer state)
    (setf (state-token-form state) (first (state-stack state))))
  (push character (state-token-buffer state)))

(defun child-role (parent child-index)
  (cond
    ((and parent
          (member (open-form-head parent) *binding-form-heads*
                  :test #'string=)
          (= child-index 2))
     :binding-list)
    ((and parent
          (eq (open-form-role parent) :binding-list))
     :binding)
    ((and parent
          (member (open-form-head parent) *clause-form-heads*
                  :test #'string=)
          (>= child-index 2))
     :clause)
    ((and parent
          (member (open-form-head parent) *slot-list-form-heads*
                  :test #'string=)
          (= child-index 4))
     :slot-list)
    ((and parent
          (eq (open-form-role parent) :slot-list))
     :slot)
    (t :ordinary)))

(defun opener-column-role-p (role)
  (member role '(:binding-list :slot-list)))

(defun role-open-threshold (role open-column)
  (case role
    (:binding-list (max 0 (1- open-column)))
    (t open-column)))

(defun make-child-open-form (parent open-column line-number)
  (let* ((child-index (when parent
                        (register-form-element parent)
                        (open-form-element-count parent)))
         (role (child-role parent child-index))
         (threshold (if (opener-column-role-p role)
                        (role-open-threshold role open-column)
                        (1+ open-column))))
    (make-open-form threshold line-number
                    :open-column open-column
                    :role role)))

(defun maybe-lower-head-continuation-threshold (form indentation)
  (when (and (open-form-head form)
             (= (open-form-element-count form) 1))
    (let ((continuation-column (max 0 (1- (open-form-open-column form)))))
      (when (and (<= continuation-column indentation)
                 (< continuation-column (open-form-column form)))
        (setf (open-form-column form) continuation-column)))))

(defun dedent-closes (state indentation)
  (loop while (state-stack state)
        for form = (first (state-stack state))
        do (maybe-lower-head-continuation-threshold form indentation)
        while (> (open-form-column form) indentation)
        do (pop (state-stack state))
        count 1))

(defun append-closes-to-previous-line (processed-lines count)
  (when (and (plusp count) processed-lines)
    (let ((target (find-if-not #'blank-line-p processed-lines)))
      (when target
        (setf (car (member target processed-lines :test #'eq))
              (concatenate 'string
                           target
                           (make-string count :initial-element #\)))))))
  processed-lines)

(defun last-code-character-index (line)
  (loop for index downfrom (1- (length line)) to 0
        unless (member (char line index) '(#\Space #\Tab))
          do (return index)))

(defun trailing-close-count (line)
  (loop with index = (last-code-character-index line)
        while (and index
                   (>= index 0)
                   (char= (char line index) #\)))
        count 1
        do (decf index)))

(defun remove-one-trailing-close (line)
  (let ((index (last-code-character-index line)))
    (if (and index
             (char= (char line index) #\)))
        (concatenate 'string
                     (subseq line 0 index)
                     (subseq line (1+ index)))
        line)))

(defun tail-events (events count)
  (last events (min count (length events))))

(defun defer-pending-trail-closes
    (state processed-lines pending-events indentation previous-indentation)
  (loop while (and pending-events
                   previous-indentation
                   (> indentation previous-indentation)
                   (<= (open-form-column
                        (close-event-open-form (first pending-events)))
                       indentation))
        do (progn
             (push (close-event-open-form (first pending-events))
                   (state-stack state))
             (when processed-lines
               (setf (first processed-lines)
                     (remove-one-trailing-close (first processed-lines))))
             (pop pending-events)))
  pending-events)

(defun process-line (line state &optional (line-number 0))
  (let (close-events)
    (values
     (with-output-to-string (output)
       (loop for character across line
             for column from 0
             do (cond
               ((plusp (state-block-comment-depth state))
                (write-char character output)
                (cond
                  ((and (state-block-sharp-seen state)
                        (char= character #\|))
                   (incf (state-block-comment-depth state))
                   (setf (state-block-sharp-seen state) nil
                         (state-block-bar-seen state) nil))
                  ((and (state-block-bar-seen state)
                        (char= character #\#))
                   (decf (state-block-comment-depth state))
                   (setf (state-block-sharp-seen state) nil
                         (state-block-bar-seen state) nil))
                  (t
                   (setf (state-block-sharp-seen state)
                         (char= character #\#)
                         (state-block-bar-seen state)
                         (char= character #\|)))))
               ((state-char-literal state)
                (write-char character output)
                (setf (state-char-literal state) nil))
               ((and (state-sharp-seen state) (char= character #\\))
                (write-char character output)
                (setf (state-sharp-seen state) nil
                      (state-char-literal state) t))
               ((state-sharp-seen state)
                (setf (state-sharp-seen state) nil)
                (cond
                  ((char= character #\")
                   (write-char character output)
                   (setf (state-in-string state)
                         (not (state-in-string state))))
                  ((char= character #\|)
                   (write-char character output)
                   (setf (state-block-comment-depth state) 1))
                  ((char= character #\;)
                   (write-char character output))
                  ((char= character #\()
                   (write-char character output)
                   (push (make-child-open-form
                          (first (state-stack state)) column line-number)
                         (state-stack state)))
                  ((char= character #\))
                   (finish-token state)
                   (if (state-stack state)
                       (let ((open-form (pop (state-stack state))))
                         (push (make-close-event open-form column)
                               close-events)
                         (write-char character output))
                       (incf (state-unmatched-closes state))))
                  (t (write-char character output))))
               ((state-escape state)
                (write-char character output)
                (setf (state-escape state) nil))
               ((and (state-in-string state) (char= character #\\))
                (write-char character output)
                (setf (state-escape state) t))
               ((char= character #\")
                (unless (state-in-string state)
                  (finish-token state))
                (write-char character output)
                (setf (state-in-string state)
                      (not (state-in-string state))))
               ((and (not (state-in-string state)) (char= character #\#))
                (finish-token state)
                (write-char character output)
                (setf (state-sharp-seen state) t))
               ((and (not (state-in-string state)) (char= character #\;))
                (finish-token state)
                (write-string line output :start column)
                (loop-finish))
               ((and (not (state-in-string state))
                     (member character '(#\Space #\Tab)))
                (finish-token state)
                (write-char character output))
               ((and (not (state-in-string state)) (char= character #\())
                (finish-token state)
                (write-char character output)
                (push (make-child-open-form
                       (first (state-stack state)) column line-number)
                      (state-stack state)))
               ((and (not (state-in-string state)) (char= character #\)))
                (finish-token state)
                (if (state-stack state)
                    (let ((open-form (pop (state-stack state))))
                      (push (make-close-event open-form column) close-events)
                      (write-char character output))
                    (incf (state-unmatched-closes state))))
               ((and (not (state-in-string state))
                     (token-character-p character))
                (begin-or-continue-token state character)
                (write-char character output))
               (t (write-char character output))))
       (finish-token state)
       (setf (state-escape state) nil
             (state-sharp-seen state) nil
             (state-char-literal state) nil
             (state-block-sharp-seen state) nil
             (state-block-bar-seen state) nil))
     (nreverse close-events))))

(defun append-remaining-closes (state processed-lines)
  (append-closes-to-previous-line processed-lines
                                  (length (state-stack state))))

(defun source-balanced-p (text)
  "Return true when TEXT has no paren balance problem this pass can repair."
  (let ((state (make-state)))
    (loop for line in (split-lines text)
          for line-number from 1
          do (process-line line state line-number))
    (and (null (state-stack state))
         (zerop (state-unmatched-closes state))
         (zerop (state-block-comment-depth state))
         (not (state-in-string state))
         (not (state-escape state))
         (not (state-sharp-seen state))
         (not (state-char-literal state))
         (not (state-block-sharp-seen state))
         (not (state-block-bar-seen state)))))

(defun indent-mode-candidate (text)
  "Return the indentation-driven repair candidate for TEXT.

Open forms close when indentation decreases or at EOF. Unmatched closing
parentheses are dropped. Parentheses in strings, line comments, and character
literals and block comments are ignored. This is the speculative Parinfer
candidate, not necessarily a safe edit."
  (let ((ends-with-newline
          (and (plusp (length text))
               (char= (char text (1- (length text))) #\Newline)))
        (state (make-state))
        (processed-lines nil)
        (pending-trail-events nil)
        (pending-trail-indentation nil)
        (blank-boundary-seen-p nil))
    (loop for line in (split-lines text)
          for line-number from 1
          do (cond
               ((and (not (state-in-string state))
                     (zerop (state-block-comment-depth state))
                     (blank-line-p line))
                (setf blank-boundary-seen-p t)
                (push line processed-lines))
               (t
                (let ((line-started-in-string-p (state-in-string state))
                      (line-started-in-block-comment-p
                        (plusp (state-block-comment-depth state))))
                  (unless (or line-started-in-string-p
                              line-started-in-block-comment-p
                              (empty-or-comment-line-p line))
                    (when (and blank-boundary-seen-p
                               (leftmost-opening-line-p line))
                      (append-closes-to-previous-line
                       processed-lines (length (state-stack state)))
                      (setf (state-stack state) nil
                            pending-trail-events nil
                            pending-trail-indentation nil))
                    (setf pending-trail-events
                          (defer-pending-trail-closes
                           state processed-lines pending-trail-events
                           (count-leading-space line)
                           pending-trail-indentation))
                    (append-closes-to-previous-line
                     processed-lines
                     (dedent-closes state (count-leading-space line))))
                  (setf blank-boundary-seen-p nil)
                  (multiple-value-bind (processed-line close-events)
                      (process-line line state line-number)
                    (push processed-line processed-lines)
                    (if (or line-started-in-string-p
                            line-started-in-block-comment-p)
                        (setf pending-trail-events nil
                              pending-trail-indentation nil)
                        (setf pending-trail-events
                              (reverse
                               (tail-events close-events
                                            (trailing-close-count line)))
                              pending-trail-indentation
                              (count-leading-space line))))))))
    (append-remaining-closes state processed-lines)
    (let ((result (format nil "~{~A~^~%~}" (nreverse processed-lines))))
      (if ends-with-newline
          (concatenate 'string result (string #\Newline))
          result))))

(defun analyze-indent-mode (text)
  "Analyze TEXT and return an INDENT-MODE-REPORT.

The report separates source balance from the indentation-driven candidate so
callers can surface reader-balanced indentation conflicts without treating them
as safe rewrites."
  (let* ((source-balanced-p (source-balanced-p text))
         (candidate (indent-mode-candidate text))
         (candidate-changed-p (not (string= text candidate)))
         (candidate-balanced-p (source-balanced-p candidate)))
    (make-indent-mode-report
     :source-balanced-p source-balanced-p
     :candidate candidate
     :candidate-balanced-p candidate-balanced-p
     :candidate-changed-p candidate-changed-p)))

(defun apply-indent-mode (text)
  "Repair TEXT only when it has a validated paren-balance problem.

Balanced source is returned unchanged, even when indentation suggests a
different tree. Use ANALYZE-INDENT-MODE or INDENT-MODE-CANDIDATE to inspect
those suspicious-but-balanced cases."
  (let ((report (analyze-indent-mode text)))
    (if (and (not (indent-mode-report-source-balanced-p report))
             (indent-mode-report-candidate-balanced-p report))
        (indent-mode-report-candidate report)
        text)))
