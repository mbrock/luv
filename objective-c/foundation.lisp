;;;; Foundation vocabulary needed by the general Objective-C boundary.

(in-package #:luv.objective-c)

(define-objective-c-message %new-autorelease-pool
    ("new" :object :ownership :owned :class "NSAutoreleasePool"))

(define-objective-c-message %drain-autorelease-pool
    ("drain" :void :consumes-receiver t))

(define-objective-c-message %retain-objective-c-object
    ("retain" :object :ownership :owned))

(define-objective-c-message %release-objective-c-object
    ("release" :void :consumes-receiver t))

(define-objective-c-message %objective-c-string-utf8-pointer
    ("UTF8String" :pointer))

(defun retain-objective-c-object (object)
  "Acquire one retain on OBJECT and return a distinct owned wrapper for it."
  (%retain-objective-c-object object))

(defun release-objective-c-object (object)
  "Consume OBJECT's one owned retain and make that wrapper unusable."
  (%release-objective-c-object object)
  (values))

(defmacro with-owned-objective-c-object ((variable form) &body body)
  "Bind VARIABLE to FORM's owned object and release it when BODY leaves."
  `(let ((,variable ,form))
     (check-consumable-objective-c-receiver ,variable)
     (unwind-protect (progn ,@body)
       (unless (objective-c-object-released-p ,variable)
         (release-objective-c-object ,variable)))))

(defmacro with-autorelease-pool (() &body body)
  "Run BODY inside one explicitly drained Foundation autorelease pool."
  (let ((pool (gensym "POOL")))
    `(let ((,pool
             (%new-autorelease-pool
              (find-objective-c-class "NSAutoreleasePool"))))
       (unwind-protect (progn ,@body)
         (%drain-autorelease-pool ,pool)))))

(defun objective-c-string (object)
  "Copy an NSString-compatible OBJECT into a Lisp string."
  (let ((pointer (%objective-c-string-utf8-pointer object)))
    (unless (cffi:null-pointer-p pointer)
      (cffi:foreign-string-to-lisp pointer))))
