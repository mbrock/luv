;;; A small application-wide semantic status line.
;;;
;;; The render thread calls REFRESH-STATUS-BAR once per frame.  That hot path
;;; increments one counter and reads the monotonic clock; semantic sampling is
;;; capped, throttled, and contains no transport or subprocess work.  McCLIM
;;; retains analytic media and Slug text which the application's final GPU pass
;;; evaluates directly at the drawable's native resolution.

(in-package #:mcluv)

(defconstant +status-bar-height+ 28)
(defconstant +status-bar-text-size+ 12)
(defconstant +status-bar-horizontal-pad+ 12)
(defconstant +status-bar-maximum-channels+ 12)
(defconstant +status-bar-maximum-field-characters+ 48)

(defparameter +status-bar-field-separator+ "  ·  ")

(defparameter *status-bar-sample-seconds* 1/2
  "Minimum time between complete semantic status samples.")

(defun status-bar-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

(defparameter *status-bar-panel-ink*
  (status-bar-alpha-ink 0.010 0.012 0.014 0.72))
(defparameter *status-bar-edge-ink*
  (status-bar-alpha-ink 0.68 0.72 0.70 0.19))
(defparameter *status-bar-text-ink* (make-rgb-color 0.86 0.88 0.86))

(defparameter +status-bar-base-channels+
  '(:application :pid :fps :heap :lobby :worktree)
  "The application-neutral status vocabulary, in presentation order.")

(defgeneric status-bar-channels-for (owner)
  (:documentation
   "Return OWNER's ordered status channel symbols.

The default method supplies the application channels.  A game adds sparse
fields with an ordinary method which appends to CALL-NEXT-METHOD; it never has
to replace the status frame or its sampling and presentation policy."))

(defmethod status-bar-channels-for ((owner t))
  (declare (ignore owner))
  (copy-list +status-bar-base-channels+))

(defgeneric status-bar-channel-label (channel owner)
  (:documentation "Return CHANNEL's compact label in OWNER's status line."))

(defmethod status-bar-channel-label ((channel symbol) owner)
  (declare (ignore owner))
  (string-downcase (symbol-name channel)))

(defgeneric status-bar-channel-value (channel owner status-bar)
  (:documentation
   "Return CHANNEL's cheap semantic value for OWNER and STATUS-BAR.

Methods run only on the throttled frame-boundary sample, never during paint.
They must inspect already-owned state and must not perform I/O or wait."))

(defgeneric status-bar-application-name (owner)
  (:documentation "Return OWNER's short application identity."))

(defmethod status-bar-application-name ((owner t))
  (string-downcase (princ-to-string (type-of owner))))

(defgeneric status-bar-lobby-client (owner)
  (:documentation "Return OWNER's shared lobby client, or NIL."))

(defmethod status-bar-lobby-client ((owner t))
  (declare (ignore owner))
  nil)

(defgeneric status-bar-source-root (owner)
  (:documentation "Return OWNER's source checkout directory, or NIL."))

(defmethod status-bar-source-root ((owner t))
  (declare (ignore owner))
  (ignore-errors (asdf:system-source-directory "luv")))

(defstruct status-bar-worktree
  name
  branch)

(defvar *status-bar-worktree-cache* (make-hash-table :test #'equal))
(defvar *status-bar-worktree-cache-lock*
  (sb-thread:make-mutex :name "status bar worktree cache"))

(defun status-bar-directory-name (directory)
  (let ((components
          (pathname-directory (uiop:ensure-directory-pathname directory))))
    (princ-to-string (or (car (last components)) "checkout"))))

(defun status-bar-read-first-line (pathname)
  (handler-case
      (with-open-file (stream pathname :direction :input)
        (read-line stream nil nil))
    (error () nil)))

(defun status-bar-git-directory (root)
  (let* ((root (uiop:ensure-directory-pathname root))
         (dot-git (merge-pathnames ".git" root)))
    (cond
      ((uiop:directory-exists-p dot-git)
       (uiop:ensure-directory-pathname dot-git))
      ((uiop:file-exists-p dot-git)
       (let ((line (status-bar-read-first-line dot-git)))
         (when (and line
                    (>= (length line) 8)
                    (string-equal "gitdir: " line :end2 8))
           (uiop:ensure-directory-pathname
            (merge-pathnames (subseq line 8) root)))))
      (t nil))))

(defun status-bar-git-branch (root)
  (alexandria:when-let* ((git-directory (status-bar-git-directory root))
                         (head (status-bar-read-first-line
                                (merge-pathnames "HEAD" git-directory))))
    (let ((prefix "ref: refs/heads/"))
      (if (and (>= (length head) (length prefix))
               (string= prefix head :end2 (length prefix)))
          (subseq head (length prefix))
          (subseq head 0 (min 8 (length head)))))))

(defun discover-status-bar-worktree (root)
  "Read ROOT's worktree identity without invoking Git or another process."
  (when root
    (let ((root (uiop:ensure-directory-pathname root)))
      (make-status-bar-worktree
       :name (status-bar-directory-name root)
       :branch (status-bar-git-branch root)))))

(defun cached-status-bar-worktree (root)
  "Return ROOT's immutable worktree description, discovering it only once."
  (when root
    (let ((key (namestring (uiop:ensure-directory-pathname root))))
      (sb-thread:with-mutex (*status-bar-worktree-cache-lock*)
        (multiple-value-bind (value present-p)
            (gethash key *status-bar-worktree-cache*)
          (if present-p
              value
              (setf (gethash key *status-bar-worktree-cache*)
                    (discover-status-bar-worktree root))))))))

(defun status-bar-worktree-description (worktree)
  (when worktree
    (let ((name (status-bar-worktree-name worktree))
          (branch (status-bar-worktree-branch worktree)))
      (if branch (format nil "~A@~A" name branch) name))))

(defstruct status-bar-field
  channel
  label
  value)

(defclass status-bar-pane (transparent-gpu-application-pane) ())

(defvar *status-bar-construction-width* 1024
  "Logical width used while MAKE-EMBEDDED-STATUS-BAR realizes its pane.")

(define-application-frame status-bar ()
  ((owner :initarg :owner :reader status-bar-owner)
   (logical-width :initarg :logical-width :accessor status-bar-logical-width)
   (worktree :initarg :worktree :reader status-bar-worktree)
   (visible-fields :initform nil :accessor status-bar-visible-fields)
   (fps :initform 0d0 :accessor status-bar-fps)
   (frames-since-sample :initform 0 :accessor status-bar-frames-since-sample)
   (last-sample-ticks
    :initform (get-internal-real-time) :accessor status-bar-last-sample-ticks)
   (revision :initform 0 :accessor status-bar-revision)
   (painted-revision :initform -1 :accessor status-bar-painted-revision)
   (repaint-count :initform 0 :accessor status-bar-repaint-count)
   (dirty-p :initform t :accessor status-bar-dirty-p))
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'status-bar-pane
                   :background +transparent-ink+
                   :width *status-bar-construction-width*
                   :height +status-bar-height+
                   :min-width *status-bar-construction-width*
                   :min-height +status-bar-height+
                   :max-width *status-bar-construction-width*
                   :max-height +status-bar-height+)))
  (:layouts (default bar)))

(defmethod status-bar-channel-label
    ((channel (eql :application)) owner)
  (declare (ignore channel owner))
  nil)

(defmethod status-bar-channel-value
    ((channel (eql :application)) owner (bar status-bar))
  (declare (ignore channel bar))
  (status-bar-application-name owner))

(defmethod status-bar-channel-value
    ((channel (eql :pid)) owner (bar status-bar))
  (declare (ignore channel owner bar))
  (princ-to-string (sb-posix:getpid)))

(defmethod status-bar-channel-value
    ((channel (eql :fps)) owner (bar status-bar))
  (declare (ignore channel owner))
  (if (plusp (status-bar-fps bar))
      (format nil "~D" (round (status-bar-fps bar)))
      "--"))

(defun status-bar-byte-description (bytes)
  (cond
    ((>= bytes (expt 1024 3))
     (format nil "~,1FG" (/ bytes (coerce (expt 1024 3) 'double-float))))
    ((>= bytes (expt 1024 2))
     (format nil "~DM" (round bytes (expt 1024 2))))
    ((>= bytes 1024)
     (format nil "~DK" (round bytes 1024)))
    (t (format nil "~DB" bytes))))

(defmethod status-bar-channel-value
    ((channel (eql :heap)) owner (bar status-bar))
  (declare (ignore channel owner bar))
  (status-bar-byte-description (sb-kernel:dynamic-usage)))

(defmethod status-bar-channel-value
    ((channel (eql :lobby)) owner (bar status-bar))
  (declare (ignore channel bar))
  (alexandria:if-let ((client (status-bar-lobby-client owner)))
    (multiple-value-bind (status peer-count)
        (luv.lobby:lobby-client-summary client)
      (format nil "~(~A~) ~D" status peer-count))
    "off"))

(defmethod status-bar-channel-value
    ((channel (eql :worktree)) owner (bar status-bar))
  (declare (ignore channel owner))
  (or (status-bar-worktree-description (status-bar-worktree bar)) "--"))

(defun truncate-status-bar-string (text)
  (if (> (length text) +status-bar-maximum-field-characters+)
      (concatenate
       'string
       (subseq text 0 (- +status-bar-maximum-field-characters+ 3))
       "...")
      ;; Status fields are retained across frames.  Even a short value must be
      ;; detached from a game-owned adjustable or otherwise mutable string so
      ;; later mutation cannot bypass the bar's revision publication.
      (copy-seq text)))

(defun bounded-status-bar-text (value)
  (truncate-status-bar-string
   (typecase value
     ;; TRUNCATE-STATUS-BAR-STRING reads only the retained prefix of a huge
     ;; game-supplied string; it never copies the discarded suffix.
     (string value)
     (t
      (let ((*print-circle* t)
            (*print-length* 8)
            (*print-level* 3)
            (*print-pretty* nil))
        (princ-to-string value))))))

(defun sample-status-bar-fields (bar)
  "Copy one bounded immutable semantic field list from BAR's owner."
  (let ((owner (status-bar-owner bar)))
    (loop for channel in (status-bar-channels-for owner)
          repeat +status-bar-maximum-channels+
          for label = (handler-case
                          (status-bar-channel-label channel owner)
                        (error () "error"))
          for value = (handler-case
                          (status-bar-channel-value channel owner bar)
                        (error () "!"))
          when value
            collect
            (make-status-bar-field
             :channel channel
             :label (and label (bounded-status-bar-text label))
             :value (bounded-status-bar-text value)))))

(defun status-bar-field-string (field)
  (let ((label (status-bar-field-label field))
        (value (status-bar-field-value field)))
    (if label (format nil "~A ~A" label value) value)))

(defun measured-status-bar-text-width (medium text)
  "Return TEXT's shaped advance in the status bar's one authored style."
  (nth-value
   0
   (text-size medium text
              :text-style
              (make-text-style nil nil +status-bar-text-size+))))

(defun fitted-status-bar-field-strings (bar medium)
  "Return the longest leading field sequence which fits BAR's current width.

Fields retain semantic order and one font size.  A narrow destination drops
only trailing fields; even the first field is omitted when it cannot fit, so
the text command never relies on clipping as its layout policy.  Fitting uses
the same Slug-shaped font advance as drawing, and only runs during a sparse
semantic repaint."
  (let ((available-width
          (max 0 (- (status-bar-logical-width bar)
                    (* 2 +status-bar-horizontal-pad+))))
        (accepted ""))
    (loop for field in (status-bar-visible-fields bar)
          for text = (status-bar-field-string field)
          for candidate = (if (zerop (length accepted))
                              text
                              (concatenate
                               'string accepted
                               +status-bar-field-separator+ text))
          while (<= (measured-status-bar-text-width medium candidate)
                    available-width)
          collect text
          do (setf accepted candidate))))

(defun status-bar-display-string (bar medium)
  (with-output-to-string (stream)
    (loop for text in (fitted-status-bar-field-strings bar medium)
          for first-p = t then nil
          unless first-p
            do (write-string +status-bar-field-separator+ stream)
          do (write-string text stream))))

(defmethod handle-repaint ((pane status-bar-pane) region)
  (declare (ignore region))
  (let* ((bar (pane-frame pane))
         (width (status-bar-logical-width bar)))
    (with-sheet-medium (medium pane)
      ;; Both layers are analytic direct-GPU media.  The panel's alpha reaches
      ;; the game's already-rendered color through premultiplied blending.
      (draw-analytic-rounded-rectangle*
       medium 0 0 width +status-bar-height+
       :radius 0 :ink *status-bar-panel-ink*)
      (draw-analytic-rounded-rectangle*
       medium 0 (- +status-bar-height+ 1) width +status-bar-height+
       :radius 0 :ink *status-bar-edge-ink*)
      ;; Every field intentionally shares this exact face and size.
      (draw-text* pane (status-bar-display-string bar medium)
                  +status-bar-horizontal-pad+ (/ +status-bar-height+ 2)
                  :align-y :center :text-size +status-bar-text-size+
                  :ink *status-bar-text-ink*))))

(define-condition status-bar-requires-direct-gpu (error)
  ((object :initarg :object :reader status-bar-non-gpu-object))
  (:report
   (lambda (condition stream)
     (format stream "The status bar requires retained direct GPU media, not ~S."
             (status-bar-non-gpu-object condition)))))

(define-condition status-bar-direct-presentation-violation (error)
  ((reason :initarg :reason :reader status-bar-presentation-violation-reason))
  (:report
   (lambda (condition stream)
     (format stream "The status bar violated its direct presentation contract: ~A."
             (status-bar-presentation-violation-reason condition)))))

(defun status-bar-mirror (bar &key (errorp t))
  (let* ((sheet (frame-top-level-sheet bar))
         (mirror (and sheet (sheet-direct-mirror sheet))))
    (cond
      ((typep mirror 'luv-gpu-mirror) mirror)
      (errorp (error 'status-bar-requires-direct-gpu :object mirror))
      (t nil))))

(defun validate-status-bar-direct-presentation (bar)
  (let* ((mirror (status-bar-mirror bar))
         (sheet (mirror-sheet mirror)))
    (when (mirror-texture mirror)
      (error 'status-bar-direct-presentation-violation
             :reason "the embedded mirror acquired a raster texture"))
    (dolist (painted-sheet (gpu-sheet-paint-order sheet))
      (let ((medium (gpu-sheet-presentation-medium painted-sheet)))
        (when (and (typep medium 'luv-gpu-medium)
                   (gpu-medium-fallback-report medium))
          (error 'status-bar-direct-presentation-violation
                 :reason "a pane used decomposed primitive fallbacks"))))
    (when (find-if
           (lambda (command) (typep command 'gpu-prepared-image-command))
           (gpu-mirror-prepared-commands mirror))
      (error 'status-bar-direct-presentation-violation
             :reason "a raster image command reached the prepared stream")))
  bar)

(defun repaint-status-bar (bar)
  "Publish BAR's retained semantic stream without acquiring a drawable."
  (alexandria:when-let ((mirror (status-bar-mirror bar :errorp nil)))
    (unless (and (mirror-embedded-p mirror) (null (mirror-texture mirror)))
      (error 'status-bar-requires-direct-gpu :object mirror))
    (repaint-gpu-mirror mirror)
    (validate-status-bar-direct-presentation bar)
    (setf (status-bar-dirty-p bar) nil
          (status-bar-painted-revision bar) (status-bar-revision bar))
    (incf (status-bar-repaint-count bar)))
  bar)

(defun prepare-status-bar (bar)
  "Publish BAR only when the throttled semantic snapshot actually changed."
  (let ((mirror (status-bar-mirror bar)))
    (if (or (status-bar-dirty-p bar)
            (null (gpu-mirror-prepared-commands mirror)))
        (repaint-status-bar bar)
        (prepare-gpu-mirror-compositor mirror)))
  bar)

(defun status-bar-pane-for (bar)
  (find-pane-named bar 'bar))

(defun resize-status-bar (bar logical-width)
  "Resize BAR to LOGICAL-WIDTH without changing glyph scale or pixel density."
  (let ((logical-width (max 1 (round logical-width))))
    (unless (= logical-width (status-bar-logical-width bar))
      (setf (status-bar-logical-width bar) logical-width
            (status-bar-dirty-p bar) t)
      (change-space-requirements
       (status-bar-pane-for bar)
       :width logical-width :min-width logical-width :max-width logical-width
       :height +status-bar-height+
       :min-height +status-bar-height+ :max-height +status-bar-height+
       :resize-frame t)))
  bar)

(defun refresh-status-bar (bar logical-width &key (now (get-internal-real-time)))
  "Count one presented frame and occasionally publish a bounded state sample."
  (resize-status-bar bar logical-width)
  (incf (status-bar-frames-since-sample bar))
  (let* ((before (status-bar-last-sample-ticks bar))
         (elapsed-ticks (- now before))
         (sample-ticks
           (* *status-bar-sample-seconds* internal-time-units-per-second)))
    (when (or (null (status-bar-visible-fields bar))
              (>= elapsed-ticks sample-ticks))
      ;; The realization sample has no meaningful time window yet.  Keep its
      ;; explicit "--" and publish the first real cadence half a second later.
      (when (and (status-bar-visible-fields bar) (plusp elapsed-ticks))
        (setf (status-bar-fps bar)
              (/ (* (status-bar-frames-since-sample bar)
                    (coerce internal-time-units-per-second 'double-float))
                 elapsed-ticks)))
      (setf (status-bar-frames-since-sample bar) 0
            (status-bar-last-sample-ticks bar) now)
      (let ((fields (sample-status-bar-fields bar)))
        (unless (equalp fields (status-bar-visible-fields bar))
          (setf (status-bar-visible-fields bar) fields
                (status-bar-dirty-p bar) t)
          (incf (status-bar-revision bar))))))
  bar)

(defun status-bar-screen-state (bar viewport-logical-extent)
  "Return BAR's top-edge affine in destination logical coordinates."
  (destructuring-bind (viewport-width viewport-height)
      viewport-logical-extent
    (let* ((source-width (status-bar-logical-width bar))
           (half-width (/ source-width viewport-width))
           (half-height (/ +status-bar-height+ viewport-height))
           ;; Direct McCLIM's affine follows the authored sheet convention:
           ;; clip-space -1 is the screen top and +1 is the screen bottom.
           (center-y (+ -1.0 half-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list 0.0 center-y 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defun make-embedded-status-bar
    (owner canvas context device logical-width &key (title "status"))
  "Create OWNER's textureless retained status line on its application canvas."
  (let* ((logical-width (max 1 (round logical-width)))
         (port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (worktree (cached-status-bar-worktree
                    (status-bar-source-root owner)))
         (bar nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf bar
                 (let ((*embedded-mirror-target* canvas)
                       (*embedded-mirror-context* context)
                       (*embedded-mirror-device* device)
                       (*status-bar-construction-width* logical-width))
                   (make-application-frame
                    'status-bar :frame-manager manager :enable t
                    :owner owner :logical-width logical-width
                    :worktree worktree)))
           (setf (frame-pretty-name bar) title)
           (make-gpu-frame-background-transparent bar)
           (let ((mirror (status-bar-mirror bar)))
             (unless (and (mirror-embedded-p mirror)
                          (null (mirror-texture mirror)))
               (error 'status-bar-requires-direct-gpu :object mirror)))
           (refresh-status-bar bar logical-width)
           (repaint-status-bar bar)
           (setf completed-p t)
           bar)
      (unless completed-p
        (when bar
          (ignore-errors (destroy-status-bar bar)))))))

(defun destroy-status-bar (bar)
  (when (and bar (not (eq :disowned (frame-state bar))))
    (destroy-frame bar))
  nil)
