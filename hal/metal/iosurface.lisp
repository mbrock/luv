;;;; IOSurface: a GPU-visible pixel buffer that survives a process boundary.
;;;;
;;;; A surface created here has a small integer identity that any process on
;;;; the machine can turn back into the same surface with IOSurfaceLookup, and
;;;; Metal can wrap either end as an ordinary texture.  That is the whole trick
;;;; behind rendering one luvcraft inside another: the parent creates the
;;;; surface, hands the child the number, and samples the texture the child
;;;; draws into.

(in-package #:luv.metal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library iosurface-framework
    (:darwin (:framework "IOSurface"))))

(cffi:use-foreign-library iosurface-framework)

(cffi:defcfun ("IOSurfaceCreate" %iosurface-create :library iosurface-framework)
    :pointer
  (properties :pointer))

(cffi:defcfun ("IOSurfaceLookup" %iosurface-lookup :library iosurface-framework)
    :pointer
  (id :uint32))

(cffi:defcfun ("IOSurfaceGetID" iosurface-id :library iosurface-framework)
    :uint32
  (surface :pointer))

(cffi:defcfun ("IOSurfaceGetWidth" iosurface-width :library iosurface-framework)
    :size
  (surface :pointer))

(cffi:defcfun ("IOSurfaceGetHeight" iosurface-height :library iosurface-framework)
    :size
  (surface :pointer))

(cffi:defcfun ("IOSurfaceGetBytesPerRow" iosurface-bytes-per-row
               :library iosurface-framework)
    :size
  (surface :pointer))

(cffi:defcfun ("IOSurfaceGetBaseAddress" iosurface-base-address
               :library iosurface-framework)
    :pointer
  (surface :pointer))

(cffi:defcfun ("IOSurfaceLock" %iosurface-lock :library iosurface-framework)
    :int32
  (surface :pointer)
  (options :uint32)
  (seed :pointer))

(cffi:defcfun ("IOSurfaceUnlock" %iosurface-unlock :library iosurface-framework)
    :int32
  (surface :pointer)
  (options :uint32)
  (seed :pointer))

(cffi:defcfun ("CFRelease" %cf-release :library luv.objective-c::foundation-framework)
    :void
  (object :pointer))

(defconstant +iosurface-lock-read-only+ 1)

(defun iosurface-property-key (name)
  "The CFStringRef constant NAME (kIOSurfaceWidth and friends) as a pointer."
  (let ((symbol (cffi:foreign-symbol-pointer name :library 'iosurface-framework)))
    (unless symbol
      (error "IOSurface does not export ~A." name))
    (cffi:mem-ref symbol :pointer)))

(objc:define-objective-c-message %new-mutable-dictionary
    ("new" :object :ownership :owned :class "NSMutableDictionary"))

(objc:define-objective-c-message %dictionary-set-object
    ("setObject:forKey:" :void)
  (object :object)
  (key :pointer))

(objc:define-objective-c-message %number-with-unsigned-long
    ("numberWithUnsignedLong:" :object :ownership :borrowed :class "NSNumber")
  (value :uint64))

;; 'BGRA' as a four-character code.
(defconstant +iosurface-pixel-format-bgra+
  (logior (ash (char-code #\B) 24) (ash (char-code #\G) 16)
          (ash (char-code #\R) 8) (char-code #\A)))

(objc:define-objective-c-message %number-with-bool
    ("numberWithBool:" :object :ownership :borrowed :class "NSNumber")
  (value :int8))

(defun create-iosurface (width height &key (bytes-per-element 4)
                                           (pixel-format +iosurface-pixel-format-bgra+)
                                           (global-p t))
  "Create a WIDTH x HEIGHT IOSurface and return its raw IOSurfaceRef.
The caller owns one CF reference; release it with RELEASE-IOSURFACE.
GLOBAL-P (deprecated by Apple, still honoured) is what lets another process
find the surface by its integer ID with LOOKUP-IOSURFACE."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (properties
          (%new-mutable-dictionary
           (objc:find-objective-c-class "NSMutableDictionary")))
      (let ((number-class (objc:find-objective-c-class "NSNumber")))
        (flet ((put (key value)
                 (%dictionary-set-object
                  properties (%number-with-unsigned-long number-class value)
                  (iosurface-property-key key))))
          (put "kIOSurfaceWidth" width)
          (put "kIOSurfaceHeight" height)
          (put "kIOSurfaceBytesPerElement" bytes-per-element)
          (put "kIOSurfacePixelFormat" pixel-format))
        (when global-p
          (%dictionary-set-object
           properties (%number-with-bool number-class 1)
           (iosurface-property-key "kIOSurfaceIsGlobal"))))
      (let ((surface (%iosurface-create (objc:objective-c-pointer properties))))
        (when (cffi:null-pointer-p surface)
          (error "IOSurfaceCreate refused a ~Dx~D surface." width height))
        surface))))

(defun lookup-iosurface (id)
  "Return a new owned IOSurfaceRef for the surface numbered ID, or NIL."
  (let ((surface (%iosurface-lookup id)))
    (if (cffi:null-pointer-p surface) nil surface)))

(defun release-iosurface (surface)
  (%cf-release surface)
  (values))

(defmacro with-locked-iosurface ((surface &key read-only) &body body)
  "Run BODY with SURFACE locked for CPU access to its base address."
  (let ((s (gensym "SURFACE")) (options (gensym "OPTIONS")))
    `(let ((,s ,surface)
           (,options (if ,read-only +iosurface-lock-read-only+ 0)))
       (%iosurface-lock ,s ,options (cffi:null-pointer))
       (unwind-protect (progn ,@body)
         (%iosurface-unlock ,s ,options (cffi:null-pointer))))))

(defun read-iosurface-pixel (surface x y)
  "The four bytes of the pixel at X, Y as a list, in memory order (BGRA)."
  (with-locked-iosurface (surface :read-only t)
    (let ((base (iosurface-base-address surface))
          (offset (+ (* y (iosurface-bytes-per-row surface)) (* x 4))))
      (loop for i below 4
            collect (cffi:mem-ref base :uint8 (+ offset i))))))

;;; Metal wraps a surface as a shared-storage texture.

(objc:define-objective-c-message %new-metal-texture-with-iosurface
    ("newTextureWithDescriptor:iosurface:plane:" :object :ownership :owned
     :class "MTLTexture")
  (descriptor :object)
  (surface :pointer)
  (plane :uint64))

(defun new-metal-texture-for-iosurface
    (device surface pixel-format usage &key label)
  "Create one owned Metal texture whose storage is SURFACE."
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-metal-texture-descriptor
           (objc:find-objective-c-class "MTLTextureDescriptor")))
      (%set-metal-texture-type descriptor +texture-type-2d+)
      (%set-metal-texture-pixel-format descriptor pixel-format)
      (%set-metal-texture-width descriptor (iosurface-width surface))
      (%set-metal-texture-height descriptor (iosurface-height surface))
      (%set-metal-texture-storage-mode descriptor +storage-mode-shared+)
      (%set-metal-texture-usage descriptor usage)
      (let ((texture (%new-metal-texture-with-iosurface device descriptor surface 0)))
        (when (and texture label)
          (%set-object-label texture (objc:lisp-string-to-objective-c label)))
        texture))))
