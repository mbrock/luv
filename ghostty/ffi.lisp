;;;; The small slice of libghostty-vt's C ABI used by the initial spike.

(in-package #:luv.ghostty)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libghostty-vt
    (:darwin (:or "libghostty-vt.0.dylib" "libghostty-vt.dylib"))
    (:unix (:or "libghostty-vt.so.0" "libghostty-vt.so"))
    (:windows "ghostty-vt.dll")))

(defvar *libghostty-vt-library* nil)

(defun load-libghostty-vt (&optional path)
  "Load libghostty-vt from PATH, or search for its platform soname.

PATH is useful for development builds which have not been installed. When it
is NIL, LUV_GHOSTTY_LIBRARY is consulted before the platform soname search."
  (or *libghostty-vt-library*
      (let ((override (or path (uiop:getenv "LUV_GHOSTTY_LIBRARY"))))
        (setf *libghostty-vt-library*
              (if override
                  (cffi:load-foreign-library override)
                  (cffi:use-foreign-library libghostty-vt))))))

(defun libghostty-vt-loaded-p ()
  (not (null *libghostty-vt-library*)))

(cffi:defcenum (ghostty-result :int)
  (:success 0)
  (:out-of-memory -1)
  (:invalid-value -2)
  (:out-of-space -3)
  (:no-value -4)
  (:io-error -5)
  (:limit-exceeded -6))

(cffi:defcenum (formatter-format :int)
  (:plain 0)
  (:vt 1)
  (:html 2))

(cffi:defcstruct formatter-screen-extra
  (size :size)
  (cursor :bool)
  (style :bool)
  (hyperlink :bool)
  (protection :bool)
  (kitty-keyboard :bool)
  (charsets :bool))

(cffi:defcstruct formatter-terminal-extra
  (size :size)
  (palette :bool)
  (modes :bool)
  (scrolling-region :bool)
  (tabstops :bool)
  (pwd :bool)
  (keyboard :bool)
  (screen (:struct formatter-screen-extra)))

(cffi:defcstruct formatter-terminal-options
  (size :size)
  (emit formatter-format)
  (unwrap :bool)
  (trim :bool)
  (extra (:struct formatter-terminal-extra))
  (selection :pointer))

(cffi:defcfun ("ghostty_terminal_new" %terminal-new) ghostty-result
  (allocator :pointer)
  (terminal :pointer)
  (columns :uint16)
  (rows :uint16))

(cffi:defcfun ("ghostty_terminal_free" %terminal-free) :void
  (terminal :pointer))

(cffi:defcfun ("ghostty_terminal_vt_write" %terminal-vt-write) :void
  (terminal :pointer)
  (data :pointer)
  (length :size))

(cffi:defcfun ("ghostty_formatter_terminal_new" %formatter-terminal-new)
    :int
  (allocator :pointer)
  (formatter :pointer)
  (terminal :pointer)
  (options (:struct formatter-terminal-options)))

(cffi:defcfun ("ghostty_formatter_format_alloc" %formatter-format-alloc)
    ghostty-result
  (formatter :pointer)
  (allocator :pointer)
  (output :pointer)
  (length :pointer))

(cffi:defcfun ("ghostty_formatter_free" %formatter-free) :void
  (formatter :pointer))

(cffi:defcfun ("ghostty_free" %ghostty-free) :void
  (allocator :pointer)
  (pointer :pointer)
  (length :size))
