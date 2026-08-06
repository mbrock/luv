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
  (:export #:apply-indent-mode))

(in-package #:sly-client/parinfer)

(defstruct (state (:constructor make-state ()))
  (stack nil :type list)
  (in-string nil :type boolean)
  (escape nil :type boolean)
  (sharp-seen nil :type boolean)
  (char-literal nil :type boolean))

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

(defun dedent-closes (state indentation)
  (loop while (and (state-stack state)
                   (> (first (state-stack state)) indentation))
        do (pop (state-stack state))
        count 1))

(defun append-closes-to-previous-line (processed-lines count)
  (when (and (plusp count) processed-lines)
    (setf (first processed-lines)
          (concatenate 'string
                       (first processed-lines)
                       (make-string count :initial-element #\)))))
  processed-lines)

(defun process-line (line state)
  (with-output-to-string (output)
    (loop for character across line
          for column from 0
          do (cond
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
                  ((char= character #\;)
                   (write-string line output :start column)
                   (loop-finish))
                  ((char= character #\()
                   (write-char character output)
                   (push (1+ column) (state-stack state)))
                  ((char= character #\))
                   (when (state-stack state)
                     (pop (state-stack state))
                     (write-char character output)))
                  (t (write-char character output))))
               ((state-escape state)
                (write-char character output)
                (setf (state-escape state) nil))
               ((and (state-in-string state) (char= character #\\))
                (write-char character output)
                (setf (state-escape state) t))
               ((char= character #\")
                (write-char character output)
                (setf (state-in-string state)
                      (not (state-in-string state))))
               ((and (not (state-in-string state)) (char= character #\#))
                (write-char character output)
                (setf (state-sharp-seen state) t))
               ((and (not (state-in-string state)) (char= character #\;))
                (write-string line output :start column)
                (loop-finish))
               ((and (not (state-in-string state)) (char= character #\())
                (write-char character output)
                (push (1+ column) (state-stack state)))
               ((and (not (state-in-string state)) (char= character #\)))
                (when (state-stack state)
                  (pop (state-stack state))
                  (write-char character output)))
               (t (write-char character output))))
    (setf (state-escape state) nil
          (state-sharp-seen state) nil
          (state-char-literal state) nil)))

(defun append-remaining-closes (state processed-lines)
  (append-closes-to-previous-line processed-lines
                                  (length (state-stack state))))

(defun apply-indent-mode (text)
  "Repair TEXT using a minimal, indentation-driven Parinfer-like pass.

Open forms close when indentation decreases or at EOF. Unmatched closing
parentheses are dropped. Parentheses in strings, line comments, and character
literals are ignored. This is deliberately a heuristic rather than a complete
Common Lisp reader."
  (let ((ends-with-newline
          (and (plusp (length text))
               (char= (char text (1- (length text))) #\Newline)))
        (state (make-state))
        (processed-lines nil))
    (dolist (line (split-lines text))
      (unless (empty-or-comment-line-p line)
        (append-closes-to-previous-line
         processed-lines
         (dedent-closes state (count-leading-space line))))
      (push (process-line line state) processed-lines))
    (append-remaining-closes state processed-lines)
    (let ((result (format nil "~{~A~^~%~}" (nreverse processed-lines))))
      (if ends-with-newline
          (concatenate 'string result (string #\Newline))
          result))))
