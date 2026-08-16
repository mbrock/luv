;;;; The small slice of libghostty-vt's C ABI used by the initial spike.

(in-package #:luv.ghostty)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libghostty-vt
    (:darwin (:or "libghostty-vt.0.dylib" "libghostty-vt.dylib"))
    (:unix (:or "libghostty-vt.so.0" "libghostty-vt.so"))
    (:windows "ghostty-vt.dll")))

(defvar *libghostty-vt-library* nil)

(defparameter *libghostty-vt-default-path*
  (uiop:getenv "LUV_GHOSTTY_LIBRARY")
  "Pinned libghostty-vt path captured when this system was loaded.

A saved standalone image retains this build-environment fallback so it can be
launched directly.  An explicit LOAD-LIBGHOSTTY-VT argument or the current
process environment still takes precedence.")

(defun load-libghostty-vt (&optional path)
  "Load libghostty-vt from PATH, or search for its platform soname.

PATH is useful for development builds which have not been installed. When it
is NIL, LUV_GHOSTTY_LIBRARY is consulted before the platform soname search."
  (or *libghostty-vt-library*
      (let ((override (or path
                          (uiop:getenv "LUV_GHOSTTY_LIBRARY")
                          *libghostty-vt-default-path*)))
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

(cffi:defcenum (terminal-option :int)
  (:userdata 0)
  (:write-pty 1))

(cffi:defcenum (terminal-data :int)
  (:invalid 0)
  (:columns 1)
  (:rows 2))

(cffi:defcenum (key-action :int)
  (:release 0)
  (:press 1)
  (:repeat 2))

;; Ghostty's physical keys follow the W3C KeyboardEvent.code order.  Keep this
;; list in header order: libghostty-vt deliberately exposes it as a C enum.
(cffi:defcenum (physical-key :int)
  (:unidentified 0)
  :backquote :backslash :bracket-left :bracket-right :comma
  :digit-0 :digit-1 :digit-2 :digit-3 :digit-4
  :digit-5 :digit-6 :digit-7 :digit-8 :digit-9
  :equal :intl-backslash :intl-ro :intl-yen
  :a :b :c :d :e :f :g :h :i :j :k :l :m
  :n :o :p :q :r :s :t :u :v :w :x :y :z
  :minus :period :quote :semicolon :slash
  :alt-left :alt-right :backspace :caps-lock :context-menu
  :control-left :control-right :enter :meta-left :meta-right
  :shift-left :shift-right :space :tab :convert :kana-mode :non-convert
  :delete :end :help :home :insert :page-down :page-up
  :arrow-down :arrow-left :arrow-right :arrow-up
  :num-lock
  :numpad-0 :numpad-1 :numpad-2 :numpad-3 :numpad-4
  :numpad-5 :numpad-6 :numpad-7 :numpad-8 :numpad-9
  :numpad-add :numpad-backspace :numpad-clear :numpad-clear-entry
  :numpad-comma :numpad-decimal :numpad-divide :numpad-enter
  :numpad-equal :numpad-memory-add :numpad-memory-clear
  :numpad-memory-recall :numpad-memory-store :numpad-memory-subtract
  :numpad-multiply :numpad-paren-left :numpad-paren-right
  :numpad-subtract :numpad-separator :numpad-up :numpad-down
  :numpad-right :numpad-left :numpad-begin :numpad-home :numpad-end
  :numpad-insert :numpad-delete :numpad-page-up :numpad-page-down
  :escape
  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13
  :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25
  :fn :fn-lock :print-screen :scroll-lock :pause)

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

(cffi:defcfun ("ghostty_terminal_resize" %terminal-resize) ghostty-result
  (terminal :pointer)
  (columns :uint16)
  (rows :uint16)
  (cell-width-pixels :uint32)
  (cell-height-pixels :uint32))

(cffi:defcfun ("ghostty_terminal_set" %terminal-set) ghostty-result
  (terminal :pointer)
  (option terminal-option)
  (value :pointer))

(cffi:defcfun ("ghostty_terminal_get" %terminal-get) ghostty-result
  (terminal :pointer)
  (data terminal-data)
  (output :pointer))

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

(cffi:defcfun ("ghostty_key_encoder_new" %key-encoder-new) ghostty-result
  (allocator :pointer)
  (encoder :pointer))

(cffi:defcfun ("ghostty_key_encoder_free" %key-encoder-free) :void
  (encoder :pointer))

(cffi:defcfun ("ghostty_key_encoder_setopt_from_terminal"
               %key-encoder-setopt-from-terminal) :void
  (encoder :pointer)
  (terminal :pointer))

(cffi:defcfun ("ghostty_key_encoder_encode" %key-encoder-encode)
    ghostty-result
  (encoder :pointer)
  (event :pointer)
  (output :pointer)
  (output-size :size)
  (output-length :pointer))

(cffi:defcfun ("ghostty_key_event_new" %key-event-new) ghostty-result
  (allocator :pointer)
  (event :pointer))

(cffi:defcfun ("ghostty_key_event_free" %key-event-free) :void
  (event :pointer))

(cffi:defcfun ("ghostty_key_event_set_action" %key-event-set-action) :void
  (event :pointer) (action key-action))

(cffi:defcfun ("ghostty_key_event_set_key" %key-event-set-key) :void
  (event :pointer) (key physical-key))

(cffi:defcfun ("ghostty_key_event_set_mods" %key-event-set-mods) :void
  (event :pointer) (modifiers :uint16))

(cffi:defcfun ("ghostty_key_event_set_consumed_mods"
               %key-event-set-consumed-mods) :void
  (event :pointer) (modifiers :uint16))

(cffi:defcfun ("ghostty_key_event_set_composing" %key-event-set-composing) :void
  (event :pointer) (composing :bool))

(cffi:defcfun ("ghostty_key_event_set_utf8" %key-event-set-utf8) :void
  (event :pointer) (text :pointer) (length :size))

(cffi:defcfun ("ghostty_key_event_set_unshifted_codepoint"
               %key-event-set-unshifted-codepoint) :void
  (event :pointer) (codepoint :uint32))
