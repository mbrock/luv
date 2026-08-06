;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:vulkan-spec)

(defun numeric-value (str)
  "Parses a string into a numeric value."
  (cond
    ((not str) nil)
    ((alexandria:starts-with-subseq "0x" str)
     (parse-integer str :start 2 :radix 16))
    ((ignore-errors (parse-integer str)))
    ((and (or (alexandria:ends-with #\f str)
              (alexandria:ends-with #\F str))
          (ignore-errors (parse-number:parse-number str :end (1- (length str))))))
    ;; todo: test and clean up
    ((multiple-value-bind (m matches)
         (ppcre:scan-to-strings "\\(~([0-9]+)?U(LL)?(-1)?(-2)?\\)" str)
       (when m
         ;; fixme: is this right on all platforms? (or any for that matter?)
         ;; this works: (at least on my machine)
         ;; (~0U)   should be e.g.  VK_REMAINING_MIP_LEVELS: 4294967295
         ;; (~0ULL) should be e.g.  VK_WHOLE_SIZE: 18446744073709551615
         ;; (~0U-1) should be e.g.  VK_QUEUE_FAMILY_EXTERNAL_KHR: 4294967294
         ;; (~0U-2) should be e.g.  VK_QUEUE_FAMILY_FOREIGN_EXT: 4294967293
         ;; NOTE: in version v1.2.174 they replaced the notation. e.g.: ~0U-1 is now ~1U 
         (let ((off (cond ((aref matches 2) -2)
                          ((aref matches 3) -3)
                          ((aref matches 0) (- (1+ (parse-integer (aref matches 0)))))
                          (t -1))))
           (if (aref matches 1)
              (ldb (byte 64 0) off)
              (if (= 4 (cffi:foreign-type-size :uint))
                  (ldb (byte 32 0) off)
                  (ldb (byte 64 0) off)))))))
    (t
     (error "Could not parse numeric value <~s>" str))))

(defun xps (node)
  "Extracts a trimmed string from a XPATH:NODE. If the given node does not contain a string NIL is returned."
  (let ((s (string-trim '(#\space #\tab) (xpath:string-value node))))
    (unless (string= s "") s)))

(defun attrib-names (node)
  "Returns a list of attribute names from an XPATH:NODE."
  (mapcar 'cxml-stp:local-name (cxml-stp:list-attributes node)))
