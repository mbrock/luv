;;;; A Tracy profiler client for luv's CPU zones.
;;;;
;;;; Tracy takes every name by pointer and asks about it later, when a viewer
;;;; wants to know what a zone it has already seen was called.  Nothing handed
;;;; to the profiler may therefore die with the call that handed it over, so
;;;; names and source locations live in never-freed foreign memory, interned
;;;; one copy per distinct zone rather than one per macro expansion.
;;;;
;;;; The client is on-demand: it collects nothing until a viewer connects, so
;;;; leaving the profiler started in a durable image costs a predictable
;;;; nothing rather than an ever-growing buffer.
;;;;
;;;; It is also built for delayed initialization, which buys the image control
;;;; over when the profiler starts at the price of a sharp edge: every entry
;;;; point here asserts that the profiler is running, and a failed assertion
;;;; ends the process rather than signalling something a handler could catch.
;;;; Everything below is therefore gated on *TRACY*, and that gate is the one
;;;; invariant this file cannot be sloppy about.

(in-package #:luv)

;;; Loading the client.
;;;
;;; The flake builds a client with TRACY_ON_DEMAND and TRACY_MANUAL_LIFETIME
;;; and names it in LUV_TRACY_CLIENT.  A client from somewhere else still
;;; works; it may simply start collecting the moment it is loaded, and
;;; START-TRACY notices that it has no lifetime entry points to call.

(cffi:define-foreign-library tracy-client
  (:darwin (:or "libTracyClient.dylib" "libTracyClient.0.13.1.dylib"))
  (:unix (:or "libTracyClient.so" "libTracyClient.so.0.13.1"))
  (t (:default "libTracyClient")))

(defvar *tracy-client* nil
  "The loaded Tracy client library, or NIL before anything asked for one.")

(defun tracy-client-override ()
  "Return the client LUV_TRACY_CLIENT names, when it names one that exists."
  (let ((name (uiop:getenv "LUV_TRACY_CLIENT")))
    (when (and name (plusp (length name)))
      (probe-file name))))

(defun load-tracy-client ()
  "Load the Tracy client library, preferring the one LUV_TRACY_CLIENT names."
  (or *tracy-client*
      (setf *tracy-client*
            (let ((override (tracy-client-override)))
              (if override
                  (cffi:load-foreign-library (namestring override))
                  (cffi:use-foreign-library tracy-client))))))

(defun tracy-client-available-p ()
  "Answer whether a Tracy client can be loaded, without complaining if not."
  (handler-case (and (load-tracy-client) t)
    (error () nil)))

;;; The C API.
;;;
;;; The entry points are spelled with the three leading underscores TracyC.h
;;; gives them; the Mach-O underscore on top of that is dlopen's business, not
;;; ours.

(cffi:defcstruct tracy-source-location
  (name :pointer)
  (zone-function :pointer)
  (file :pointer)
  (line :uint32)
  (color :uint32))

;;; TracyCZoneCtx is `struct { uint32_t id; int32_t active; }'.  Every ABI luv
;;; builds for returns that in the register a uint64_t would use -- id in the
;;; low half, active in the high half -- so the context travels as an opaque
;;; word.  Passing it as a structure instead would route the hot path through
;;; libffi, which costs more per zone than the measurement it is taking.
;;; LUVCRAFT.TESTS checks the claim rather than trusting it.

(cffi:defcfun ("___tracy_startup_profiler" %tracy-startup-profiler) :void)

(cffi:defcfun ("___tracy_shutdown_profiler" %tracy-shutdown-profiler) :void)

(cffi:defcfun ("___tracy_connected" %tracy-connected) :int32)

(cffi:defcfun ("___tracy_emit_zone_begin" %tracy-emit-zone-begin) :uint64
  (source-location :pointer)
  (active :int32))

(cffi:defcfun ("___tracy_emit_zone_end" %tracy-emit-zone-end) :void
  (context :uint64))

(cffi:defcfun ("___tracy_emit_zone_value" %tracy-emit-zone-value) :void
  (context :uint64)
  (value :uint64))

(cffi:defcfun ("___tracy_set_thread_name" %tracy-set-thread-name) :void
  (name :pointer))

(cffi:defcfun ("___tracy_emit_frame_mark" %tracy-emit-frame-mark) :void
  (name :pointer))

(cffi:defcfun ("___tracy_emit_plot" %tracy-emit-plot) :void
  (name :pointer)
  (value :double))

(cffi:defcfun ("___tracy_emit_plot_config" %tracy-emit-plot-config) :void
  (name :pointer)
  (format :int32)
  (step :int32)
  (fill :int32)
  (color :uint32))

(cffi:defcfun ("___tracy_emit_message" %tracy-emit-message) :void
  (text :pointer)
  (size :size)
  (callstack-depth :int32))

(cffi:defcfun ("___tracy_emit_messageC" %tracy-emit-colored-message) :void
  (text :pointer)
  (size :size)
  (color :uint32)
  (callstack-depth :int32))

(cffi:defcfun ("___tracy_emit_message_appinfo" %tracy-emit-application-info)
    :void
  (text :pointer)
  (size :size))

(defparameter *tracy-plot-formats*
  '((:number . 0) (:memory . 1) (:percentage . 2) (:watt . 3))
  "TracyPlotFormatEnum, by the keyword luv names each format with.")

;;; Interned names and source locations.

(defvar *tracy-names* (make-hash-table :test #'equal)
  "Foreign copies of every name handed to Tracy, keyed by the Lisp string.")

(defvar *tracy-source-locations* (make-hash-table :test #'equal)
  "Foreign source locations, keyed by the fields that describe them.")

(defun tracy-name (string)
  "Return a stable foreign copy of STRING.

Tracy keeps names by pointer and resolves them long after the call that
introduced them, so these copies are deliberately never freed."
  (or (gethash string *tracy-names*)
      (setf (gethash string *tracy-names*)
            (cffi:foreign-string-alloc string))))

(defun tracy-source-location
    (name &key (zone-function name) (file "") (line 0) (color 0))
  "Return a stable foreign source location describing zone NAME.

Locations are interned by their fields rather than allocated per expansion.
Recompiling a file re-runs its LOAD-TIME-VALUE forms, and Tracy tells zones
apart by the address of their location: without the table, recompiling a
function in the middle of a capture would split its zone in two."
  (let ((key (list name zone-function file line color)))
    (or (gethash key *tracy-source-locations*)
        (setf (gethash key *tracy-source-locations*)
              (let ((location
                      (cffi:foreign-alloc '(:struct tracy-source-location))))
                (flet ((fill-slot (slot value)
                         (setf (cffi:foreign-slot-value
                                location '(:struct tracy-source-location) slot)
                               value)))
                  (fill-slot 'name (tracy-name name))
                  (fill-slot 'zone-function (tracy-name zone-function))
                  (fill-slot 'file (tracy-name file))
                  (fill-slot 'line line)
                  (fill-slot 'color color))
                location)))))

(defun tracy-zone-name (designator)
  "Render a zone name DESIGNATOR the way PRINT-CPU-TRACE renders it."
  (typecase designator
    (string designator)
    (symbol (string-downcase (symbol-name designator)))
    (t (princ-to-string designator))))

(defun tracy-literal-zone-name (designator)
  "Return DESIGNATOR's zone name when a macro can already read it, else NIL."
  (typecase designator
    (string designator)
    (keyword (string-downcase (symbol-name designator)))))

;;; Lifecycle.

(defvar *tracy* nil
  "True while this image is offering zones to a Tracy viewer.")

(defun tracy-manual-lifetime-p ()
  "Answer whether the loaded client was built with TRACY_MANUAL_LIFETIME."
  (and (cffi:foreign-symbol-pointer "___tracy_startup_profiler") t))

(defun tracy-application-info (text)
  "Describe this program to any viewer that later connects."
  (cffi:with-foreign-string ((pointer size) text)
    (%tracy-emit-application-info pointer (1- size))))

(defun name-tracy-thread (name)
  "Name the calling thread in Tracy's timeline, if the profiler is running.

Worker threads are worth naming as they start: an unnamed thread still gets a
lane, but the lane is a thread id rather than a job.  A thread that starts
before START-TRACY keeps the anonymous lane, so start Tracy before the session
whose threads you want to read.

The test is on the profiler rather than on the loaded library, and that is not
a detail to relax.  A client built for delayed initialization asserts its way
out of the whole process -- not into a Lisp condition -- when any entry point
is called before startup or after shutdown."
  (when *tracy*
    (cffi:with-foreign-string (pointer name)
      (%tracy-set-thread-name pointer))))

(defun start-tracy (&key (application-name "luv"))
  "Load the Tracy client, start the profiler, and answer whether it is running.

The client collects on demand, so starting it does not begin a capture: it
makes this image discoverable, and zones start recording when a viewer
connects.  Leaving it started is the intended state for a durable image."
  (unless *tracy*
    (load-tracy-client)
    (when (tracy-manual-lifetime-p)
      (%tracy-startup-profiler))
    (setf *tracy* t)
    (name-tracy-thread "main")
    (tracy-application-info application-name))
  *tracy*)

(defun stop-tracy ()
  "Stop the Tracy profiler, if this image started one it is allowed to stop."
  (when *tracy*
    (setf *tracy* nil)
    (when (tracy-manual-lifetime-p)
      (%tracy-shutdown-profiler)))
  nil)

(defun tracy-connected-p ()
  "Answer whether a Tracy viewer is attached and therefore recording."
  (and *tracy* (plusp (%tracy-connected))))

;;; Zones.

(defmacro with-tracy-zone
    ((name &key (color 0) (value nil value-supplied-p)) &body body)
  "Measure BODY as a Tracy zone named NAME.

When VALUE is supplied, evaluate it as the zone exits and attach the resulting
unsigned integer to the zone.  This is useful for semantic work counts such as
sites visited: Tracy can then distinguish a slower realization from one which
simply performed more work.

A literal NAME -- a string or a keyword, which is every zone luv writes by
hand -- gets one lazily initialized source-location cell per macro expansion.
The cell itself may be dumped into a standalone Lisp core, but its foreign
pointer is not allocated until the restored process first enters the zone.
This distinction matters: foreign memory allocated by LOAD-TIME-VALUE does not
survive SAVE-LISP-AND-DIE even though the Lisp pointer object does.  A computed
NAME still works through luv's source-location table, which is slower but
reuses the same stable foreign record instead of growing Tracy's allocation
table on every entry."
  (let ((context (gensym "CONTEXT"))
        (location-cell (gensym "LOCATION-CELL"))
        (literal (tracy-literal-zone-name name))
        (file (if *compile-file-truename*
                  (namestring *compile-file-truename*)
                  "")))
    `(if *tracy*
         (let* (,@(when literal
                    `((,location-cell (load-time-value (cons nil nil) nil))))
                (,context
                 ,(if literal
                      `(%tracy-emit-zone-begin
                        (or (car ,location-cell)
                            (setf (car ,location-cell)
                                  (tracy-source-location
                                   ,literal :file ,file :color ,color)))
                        1)
                      `(%tracy-emit-zone-begin
                        (tracy-source-location
                         (tracy-zone-name ,name) :file ,file :color ,color)
                        1))))
            (unwind-protect
                 (progn ,@body)
              ,@(when value-supplied-p
                  `((%tracy-emit-zone-value ,context ,value)))
              (%tracy-emit-zone-end ,context)))
         (progn ,@body))))

;;; Frames, plots, and messages.

(defun tracy-frame-mark (&optional name)
  "End the current Tracy frame, or the secondary frame set called NAME.

Marking frames is what gives the viewer its frame-time graph, and what lets it
say which frame a zone belongs to."
  (when *tracy*
    (%tracy-emit-frame-mark
     (if name (tracy-name name) (cffi:null-pointer)))))

(defun tracy-plot (name value)
  "Record VALUE on Tracy's plot NAME, drawn against the same timeline."
  (when *tracy*
    (%tracy-emit-plot (tracy-name name) (coerce value 'double-float))))

(defun configure-tracy-plot
    (name &key (format :number) (step t) (fill nil) (color 0))
  "Describe how the viewer should draw plot NAME.

STEP suits a quantity that holds its value between changes, such as a count of
resident chunks, rather than one that is sampled continuously."
  (when *tracy*
    (%tracy-emit-plot-config
     (tracy-name name)
     (or (cdr (assoc format *tracy-plot-formats*))
         (error "Unknown Tracy plot format ~S." format))
     (if step 1 0)
     (if fill 1 0)
     color)))

(defun tracy-message (text &key color)
  "Post TEXT to the viewer's message log at this instant on the timeline."
  (when *tracy*
    (cffi:with-foreign-string ((pointer size) text)
      (if color
          (%tracy-emit-colored-message pointer (1- size) color 0)
          (%tracy-emit-message pointer (1- size) 0)))))
