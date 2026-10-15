;;;; Ratpoison in Luvland: a prefix key, keymaps, and CLIM commands.
;;;;
;;;; Commands are CLIM commands in the LUVLAND-COMMANDS table, executed on a
;;;; headless application frame, so the workbench's M-x and legend read the
;;;; same vocabulary.  Keymaps follow StumpWM (#5JDNKF): they name each other
;;;; by symbol, and a binding is a command line such as "select 3", parsed
;;;; against the table, with missing arguments asked for at the prompt.  Key
;;;; sequences are read as Mahogany reads them (#BCG9SH), one event at a
;;;; time, never by blocking the canvas thread.

(in-package #:luvland)

;;; ---------------------------------------------------------------------
;;; The command frame.

(clim:define-command-table luvland-commands)

(clim:define-application-frame luvland-frame ()
  ((luvland :initarg :luvland :reader frame-luvland))
  (:command-table (luvland-frame :inherit-from (luvland-commands)))
  (:menu-bar nil)
  (:panes)
  (:layouts (default nil)))

(defun current-luvland ()
  (frame-luvland clim:*application-frame*))

(defmacro define-luvland-command (name (&rest arguments) &body body)
  "Define command NAME, a symbol whose lowercase name is its command line,
with CLIM ARGUMENTS such as (NUMBER 'INTEGER).  BODY runs on the canvas
thread with LUVLAND bound."
  `(clim:define-command (,(intern (format nil "COM-~A" (symbol-name name)))
                         :command-table luvland-commands
                         :name ,(string-downcase (symbol-name name)))
       ,arguments
     (let ((luvland (current-luvland)))
       (declare (ignorable luvland))
       ,@body)))

(defun luvland-workbench (luvland)
  (luv.workbench:application-workbench luvland))

(defun message (luvland format-control &rest arguments)
  "Show a message in LUVLAND's message window, as StumpWM's MESSAGE does."
  (let ((text (apply #'format nil format-control arguments))
        (workbench (luvland-workbench luvland)))
    (if workbench
        (luv.workbench:show-workbench-message workbench text)
        (format *error-output* "~&luvland: ~A~%" text))
    text))

(defun error-message (luvland format-control &rest arguments)
  (let ((text (apply #'format nil format-control arguments))
        (workbench (luvland-workbench luvland)))
    (if workbench
        (luv.workbench:show-workbench-message workbench text :error-p t :seconds 8)
        (format *error-output* "~&luvland: ~A~%" text))
    text))

(defun show-pending (luvland text)
  "Show TEXT until the key sequence ends; nothing without a workbench."
  (let ((workbench (luvland-workbench luvland)))
    (when workbench
      (luv.workbench:show-workbench-message workbench text :seconds nil))))

(defun clear-message (luvland)
  (let ((workbench (luvland-workbench luvland)))
    (when workbench
      (luv.workbench:clear-workbench-message workbench))))

(defun prompt (luvland label function &key (initial "") completer)
  "Read a line at LUVLAND's prompt and call FUNCTION with it, later."
  (luv.workbench:open-workbench-prompt (luvland-workbench luvland)
                                       :label label :initial initial
                                       :completer completer :function function))

;;; ---------------------------------------------------------------------
;;; Command lines.

(defun command-names ()
  "Luvland's command-line names and their command symbols."
  (let ((names '()))
    (clim:map-over-command-table-names
     (lambda (name symbol) (push (cons name symbol) names))
     'luvland-commands :inherited nil)
    (sort names #'string< :key #'car)))

(defun find-command (name)
  (cdr (assoc name (command-names) :test #'string-equal)))

(defun command-arguments (symbol)
  "Each required argument of command SYMBOL as (name presentation-type)."
  (mapcar (lambda (description)
            (list (first description) (eval (second description))))
          (climi::required-args (gethash symbol climi::*command-parser-table*))))

(defun split-first-word (text)
  (let* ((text (string-trim " " text))
         (space (position #\Space text)))
    (if space
        (values (subseq text 0 space) (string-left-trim " " (subseq text space)))
        (values text ""))))

(defun execute-command (luvland command)
  "Run COMMAND, a list (symbol . arguments), on LUVLAND's frame.  An error
becomes a message, as in StumpWM, rather than a parked canvas."
  (handler-case (clim:execute-frame-command (luvland-command-frame luvland) command)
    (error (condition)
      (error-message luvland "~A: ~A" (string-downcase (first command)) condition))))

(defun run-command-line (luvland line)
  "Parse LINE, such as \"select 3\" or \"exec foot -e htop\", and run it.
Each argument is one word, except a final string argument, which takes the
rest of the line; arguments the line lacks are read at the prompt."
  (multiple-value-bind (name rest) (split-first-word line)
    (let ((symbol (find-command name)))
      (if (null symbol)
          (error-message luvland "No command \"~A\"." name)
          (labels ((finish (arguments)
                     (execute-command luvland (cons symbol (reverse arguments))))
                   (parse (specifications text arguments)
                     (if (null specifications)
                         (finish arguments)
                         (destructuring-bind (argument type) (first specifications)
                           (multiple-value-bind (word remainder)
                               (if (and (null (rest specifications)) (eq type 'string))
                                   (values (string-trim " " text) "")
                                   (split-first-word text))
                             (if (string= word "")
                                 (prompt luvland
                                         (format nil "~(~A~):" argument)
                                         (lambda (answer)
                                           (parse specifications answer arguments)))
                                 (handler-case
                                     (parse (rest specifications) remainder
                                            (cons (if (eq type 'string)
                                                      word
                                                      (clim:accept-from-string type word))
                                                  arguments))
                                   (error ()
                                     (error-message luvland "~A is not a ~(~A~) for ~(~A~)."
                                                    word type argument)))))))))
            (parse (command-arguments symbol) rest '()))))))

(defun run-command (line &optional (luvland *luvland*))
  "Run command LINE in LUVLAND from any thread, such as Sly's."
  (luv:request-canvas-frame (luvland-canvas luvland)
                            (lambda (timestamp)
                              (declare (ignore timestamp))
                              (run-command-line luvland line))))

;;; ---------------------------------------------------------------------
;;; Keys and keymaps.

(defparameter +key-names+
  '(("RET" . :return) ("SPC" . #\Space) ("TAB" . :tab) ("ESC" . :escape)
    ("DEL" . :backspace) ("BackSpace" . :backspace) ("Delete" . :delete)
    ("Left" . :left) ("Right" . :right) ("Up" . :up) ("Down" . :down)
    ("Home" . :home) ("End" . :end) ("Prior" . :page-up) ("Next" . :page-down))
  "kbd's names for keys that are not one printing character, as in StumpWM.")

(defparameter +modifier-prefixes+
  '((#\C . :control) (#\M . :meta) (#\s . :super) (#\S . :shift)))

(defparameter +modifier-order+ '(:control :meta :super :shift))

(defun canonical-modifiers (modifiers)
  (remove-if-not (lambda (modifier) (member modifier modifiers)) +modifier-order+))

(defun make-key (base modifiers)
  (cons base (canonical-modifiers modifiers)))

(defun kbd (string)
  "The key STRING names, in StumpWM's syntax: \"C-t\", \"M-Left\", \"RET\",
\"!\".  Modifiers are C- control, M- meta, s- super, and S- shift."
  (let ((modifiers '())
        (rest string))
    (loop while (and (> (length rest) 2) (char= (char rest 1) #\-)
                     (assoc (char rest 0) +modifier-prefixes+))
          do (push (cdr (assoc (char rest 0) +modifier-prefixes+)) modifiers)
             (setf rest (subseq rest 2)))
    (let ((base (cond ((= (length rest) 1) (char rest 0))
                      ((cdr (assoc rest +key-names+ :test #'string=)))
                      ((and (char-equal (char rest 0) #\F) (every #'digit-char-p (subseq rest 1)))
                       (intern (string-upcase rest) :keyword))
                      (t (error "kbd does not know the key ~S." rest)))))
      (make-key base modifiers))))

(defun print-key (key)
  (format nil "~{~A-~}~A"
          (mapcar (lambda (modifier) (car (rassoc modifier +modifier-prefixes+)))
                  (rest key))
          (let ((base (first key)))
            (cond ((eql base #\Space) "SPC")
                  ((characterp base) (string base))
                  ((car (rassoc base +key-names+)))
                  (t (string-capitalize base))))))

(defun event-key (event)
  "The key EVENT pressed.  A printing key is named by its character in the
current layout, so C-t is C-t on Dvorak; with control, meta, or super that
is the unshifted character."
  (let* ((modifiers (luv:canvas-key-event-modifiers event))
         (strong (intersection '(:control :meta :super) modifiers))
         (character (if strong
                        (luv:canvas-key-event-unshifted-character event)
                        (luv:canvas-key-event-character event))))
    (if (and character (graphic-char-p character))
        (make-key character strong)
        (make-key (luv:canvas-key-event-key-name event)
                  (intersection '(:control :meta :super :shift) modifiers)))))

(defparameter +modifier-key-names+
  '(:control-left :control-right :shift-left :shift-right :alt-left :alt-right
    :super-left :super-right :caps-lock :num-lock :menu))

(defstruct (keymap (:constructor make-sparse-keymap ()))
  (bindings (make-hash-table :test 'equal))
  (order '()))

(defmethod print-object ((keymap keymap) stream)
  (print-unreadable-object (keymap stream :type t)
    (format stream "~D binding~:P" (hash-table-count (keymap-bindings keymap)))))

(defun define-key (keymap key binding)
  "Bind KEY in KEYMAP to BINDING: a command line, a keymap, a symbol naming
a keymap, or NIL to remove the binding."
  (let ((bindings (keymap-bindings keymap)))
    (if binding
        (progn (unless (gethash key bindings)
                 (setf (keymap-order keymap) (append (keymap-order keymap) (list key))))
               (setf (gethash key bindings) binding))
        (progn (remhash key bindings)
               (setf (keymap-order keymap) (remove key (keymap-order keymap) :test #'equal))))
    binding))

(defun resolve-keymap (binding)
  (cond ((keymap-p binding) binding)
        ((and (symbolp binding) (boundp binding) (keymap-p (symbol-value binding)))
         (symbol-value binding))))

(defun lookup-key (keymap key)
  (gethash key (keymap-bindings keymap)))

(defvar *top-map* (make-sparse-keymap)
  "Keys Luvland takes from clients at any time.")
(defvar *root-map* (make-sparse-keymap)
  "Keys after the prefix key.")
(defvar *prefix-key* (kbd "C-t"))

(defun set-prefix-key (key)
  "Make KEY the prefix, as StumpWM's SET-PREFIX-KEY does."
  (define-key *top-map* *prefix-key* nil)
  (define-key *root-map* *prefix-key* nil)
  (setf *prefix-key* key)
  (define-key *top-map* key '*root-map*)
  (define-key *root-map* key "other")
  key)

;;; ---------------------------------------------------------------------
;;; Reading key sequences.  LUVLAND-PENDING-KEYMAPS holds the keymaps a
;;; sequence may continue in, NIL at top level.

(defparameter *help-keys* (list (kbd "?") (kbd "C-h")))

(defun pending-description (luvland)
  (format nil "~{~A~^ ~} -" (mapcar #'print-key (reverse (luvland-key-sequence luvland)))))

(defun reset-key-state (luvland)
  (setf (luvland-pending-keymaps luvland) nil
        (luvland-key-sequence luvland) nil))

(defun handle-command-key (luvland event)
  "Read EVENT as part of a key sequence.  Return true when Luvland took the
key, and NIL when it belongs to the focused client."
  (let ((key-name (luv:canvas-key-event-key-name event)))
    (when (member key-name +modifier-key-names+)
      ;; Modifiers always reach clients; they may also begin a chord here.
      (return-from handle-command-key nil))
    (let* ((key (event-key event))
           (pending (luvland-pending-keymaps luvland))
           (keymaps (or pending (list *top-map*)))
           (bindings (remove nil (mapcar (lambda (keymap) (lookup-key keymap key)) keymaps)))
           (binding (first bindings)))
      (cond
        ((and pending (luv:canvas-key-event-repeat-p event)) t)
        ((resolve-keymap binding)
         (when (null pending)
           (setf (luvland-prefix-key-name luvland) key-name))
         (setf (luvland-pending-keymaps luvland)
               (remove nil (mapcar #'resolve-keymap bindings)))
         (push key (luvland-key-sequence luvland))
         (show-pending luvland (pending-description luvland))
         t)
        (binding
         (reset-key-state luvland)
         (clear-message luvland)
         (if (functionp binding)
             (funcall binding luvland)
             (run-command-line luvland binding))
         t)
        ((null pending) nil)
        ((member key *help-keys* :test #'equal)
         (show-keymap-help luvland pending (pending-description luvland))
         (reset-key-state luvland)
         t)
        ((equal key (kbd "C-g"))
         (reset-key-state luvland)
         (clear-message luvland)
         t)
        (t
         (let ((sequence (pending-description luvland)))
           (reset-key-state luvland)
           (message luvland "~A ~A is not bound."
                    (string-right-trim " -" sequence) (print-key key)))
         t)))))

(defun send-prefix-key (luvland)
  "Send the prefix key itself to the focused client, as ratpoison's C-t t."
  (let ((code (key-evdev-code (luvland-prefix-key-name luvland)))
        (control-p (member :control (rest *prefix-key*)))
        (meta-p (member :meta (rest *prefix-key*))))
    (when code
      (send-to-server luvland
                      (lambda ()
                        (when control-p (wl:send-key 29 t))
                        (when meta-p (wl:send-key 56 t))
                        (wl:send-key code t)
                        (wl:send-key code nil)
                        (when meta-p (wl:send-key 56 nil))
                        (when control-p (wl:send-key 29 nil)))))))

;;; Help: the keymap legend, fed with rows read off a keymap.

(defun keymap-rows (keymap prefix)
  (let ((rows '()))
    (dolist (key (keymap-order keymap))
      (let* ((binding (lookup-key keymap key))
             (label (cond ((stringp binding) binding)
                          ((resolve-keymap binding) "prefix")
                          ((functionp binding) "function")
                          (t (princ-to-string binding))))
             (row (assoc label rows :test #'string=))
             (keys (format nil "~@[~A ~]~A" prefix (print-key key))))
        (if row
            (setf (cdr row) (append (cdr row) (list keys)))
            (push (list label keys) rows))))
    (nreverse rows)))

(defmethod mcluv:keymap-legend-sections-for ((luvland luvland))
  (let ((prefix (print-key *prefix-key*)))
    (or (luvland-help-sections luvland)
        (list (cons (format nil "After ~A" prefix) (keymap-rows *root-map* prefix))
              (cons "Any time" (remove "prefix" (keymap-rows *top-map* nil)
                                       :key #'first :test #'string=))))))

(defun show-keymap-help (luvland keymaps title)
  (setf (luvland-help-sections luvland)
        (loop for keymap in keymaps
              collect (cons title (keymap-rows keymap (string-right-trim " -" title)))))
  (unwind-protect (luv.workbench:open-workbench-keymap (luvland-workbench luvland))
    (setf (luvland-help-sections luvland) nil)))

;;; ---------------------------------------------------------------------
;;; Window numbers and recency, as StumpWM keeps them.

(defun window-by-number (luvland number)
  (find number (luvland-windows luvland) :key #'window-number))

(defun lowest-free-window-number (luvland)
  (loop for number from 0
        unless (window-by-number luvland number) return number))

(defun window-title (window)
  (let ((toplevel (window-toplevel window)))
    (or (wl:toplevel-title toplevel) (wl:toplevel-app-id toplevel) "")))

(defun recent-windows (luvland)
  "LUVLAND's windows, most recently focused first."
  (let ((windows (luvland-windows luvland)))
    (append (remove nil (mapcar (lambda (toplevel) (find toplevel windows :key #'window-toplevel))
                                (luvland-history luvland)))
            (remove-if (lambda (window) (member (window-toplevel window) (luvland-history luvland)))
                       windows))))

(defun format-window-list (luvland)
  (let* ((recent (recent-windows luvland))
         (current (first recent))
         (last (second recent)))
    (if (null recent)
        (list "No managed windows")
        (loop for window in (sort (copy-list (luvland-windows luvland)) #'< :key #'window-number)
              collect (format nil "~D~A ~A" (window-number window)
                              (cond ((eq window current) "*")
                                    ((eq window last) "+")
                                    (t "-"))
                              (window-title window))))))

;;; ---------------------------------------------------------------------
;;; Commands.

(define-luvland-command other ()
  (let ((other (second (recent-windows luvland))))
    (if other
        (focus-window luvland (window-toplevel other))
        (message luvland "No other window."))))

(define-luvland-command next ()
  (focus-neighbor luvland 1 :wrap t))

(define-luvland-command prev ()
  (focus-neighbor luvland -1 :wrap t))

(define-luvland-command select ((number 'integer))
  (let ((window (window-by-number luvland number)))
    (if window
        (focus-window luvland (window-toplevel window))
        (message luvland "No window ~D." number))))

(define-luvland-command windows ()
  (luv.workbench:show-workbench-message (luvland-workbench luvland)
                                        (format-window-list luvland)))

(define-luvland-command exec ((line 'string))
  (spawn-client luvland "/bin/sh" "-c" (format nil "exec ~A" line)))

(define-luvland-command terminal ()
  (apply #'spawn-client luvland *terminal*))

(define-luvland-command delete ()
  (let ((toplevel (luvland-focus luvland)))
    (when toplevel
      (wl:call-in-server (lambda () (wl:close-toplevel toplevel))
                         :server (luvland-server luvland) :wait nil))))

(define-luvland-command kill ()
  (let ((toplevel (luvland-focus luvland)))
    (when toplevel
      (wl:call-in-server (lambda () (wl:destroy-client (wl:resource-client toplevel)))
                         :server (luvland-server luvland) :wait nil))))

(define-luvland-command overview ()
  (toggle-overview luvland))

(define-luvland-command time ()
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (message luvland "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
             year month date hour minute second)))

(define-luvland-command lastmsg ()
  (let ((previous (second (mcluv:message-history
                           (luv.workbench:workbench-frame (luvland-workbench luvland))))))
    (if previous
        (luv.workbench:show-workbench-message (luvland-workbench luvland) previous)
        (message luvland "No earlier message."))))

(define-luvland-command colon ()
  (prompt luvland ":"
          (lambda (line) (unless (string= "" (string-trim " " line))
                           (run-command-line luvland line)))
          :completer (lambda (text)
                       (let ((word (string-trim " " text)))
                         (unless (find #\Space word)
                           (loop for (name) in (command-names)
                                 when (and (<= (length word) (length name))
                                           (string-equal word name :end2 (length word)))
                                   collect name))))))

(defun eval-line (luvland line)
  "Read and evaluate LINE in package LUVLAND-USER and show every value."
  (handler-case
      (let* ((*package* (find-package '#:luvland-user))
             (values (multiple-value-list (eval (read-from-string line)))))
        (message luvland "~{~S~^~%~}" (or values '("; No value"))))
    (error (condition)
      (error-message luvland "~A" condition))))

(define-luvland-command eval ()
  (prompt luvland "eval:" (lambda (line) (eval-line luvland line))))

(defun program-names (prefix)
  (let ((names '()))
    (dolist (directory (uiop:split-string (or (uiop:getenv "PATH") "") :separator ":"))
      (when (plusp (length directory))
        (dolist (file (ignore-errors (uiop:directory-files (uiop:ensure-directory-pathname directory))))
          (let ((name (file-namestring file)))
            (when (and (<= (length prefix) (length name))
                       (string= prefix name :end2 (length prefix)))
              (pushnew name names :test #'string=))))))
    (sort names #'string<)))

(define-luvland-command exec-prompt ()
  (prompt luvland "exec:"
          (lambda (line) (unless (string= "" (string-trim " " line))
                           (run-command-line luvland (format nil "exec ~A" line))))
          :completer (lambda (text)
                       (unless (or (string= text "") (find #\Space text))
                         (let ((names (program-names text)))
                           (subseq names 0 (min 40 (length names))))))))

(define-luvland-command help ()
  (luv.workbench:open-workbench-keymap (luvland-workbench luvland)))

(define-luvland-command commands ()
  (luv.workbench:open-workbench-command-menu (luvland-workbench luvland)))

(define-luvland-command send-escape ()
  (send-prefix-key luvland))

(define-luvland-command loadrc ()
  (load-init-file luvland))

(define-luvland-command abort ()
  nil)

(define-luvland-command quit ()
  (prompt luvland "Quit Luvland and its windows? (yes/no)"
          (lambda (answer)
            (when (member (string-trim " " answer) '("y" "yes") :test #'string-equal)
              (request-stop luvland)))))

;;; ---------------------------------------------------------------------
;;; Ratpoison's bindings.

(defun install-default-bindings ()
  (setf *top-map* (make-sparse-keymap)
        *root-map* (make-sparse-keymap)
        *prefix-key* (kbd "C-t"))
  (define-key *top-map* *prefix-key* '*root-map*)
  ;; The chords Luvland had before it had a prefix, for nested habits.
  (define-key *top-map* (kbd "C-M-Left") "prev")
  (define-key *top-map* (kbd "C-M-Right") "next")
  (define-key *top-map* (kbd "C-M-RET") "terminal")
  (define-key *top-map* (kbd "C-M-o") "overview")
  (define-key *top-map* (kbd "C-M-DEL") "delete")
  (loop for (key command)
          on '("c" "terminal" "C-c" "terminal"
               "!" "exec-prompt"
               ";" "colon"
               ":" "eval"
               "C-t" "other"
               "t" "send-escape"
               "n" "next" "C-n" "next" "SPC" "next"
               "p" "prev" "C-p" "prev"
               "w" "windows" "C-w" "windows"
               "k" "delete" "C-k" "delete"
               "K" "kill"
               "RET" "overview"
               "a" "time" "C-a" "time"
               "m" "lastmsg" "C-m" "lastmsg"
               "?" "help"
               "x" "commands"
               "q" "quit"
               "C-g" "abort")
        by #'cddr
        do (define-key *root-map* (kbd key) command))
  (dotimes (number 10)
    (define-key *root-map* (kbd (princ-to-string number))
      (format nil "select ~D" number))))

(install-default-bindings)

;;; ---------------------------------------------------------------------
;;; The init file.

(defpackage #:luvland-user
  (:use #:cl #:luvland)
  (:documentation "Where Luvland's init file and eval prompt read."))

(defparameter *init-files*
  (list (uiop:xdg-config-home "luvland/init.lisp")
        (merge-pathnames ".luvlandrc" (user-homedir-pathname))))

(defun load-init-file (luvland)
  "Load the first existing init file in package LUVLAND-USER, reporting how
it went as a message."
  (let ((file (find-if #'probe-file *init-files*)))
    (cond
      ((null file) nil)
      (t
       (handler-case
           (let ((*package* (find-package '#:luvland-user)))
             (load file)
             (message luvland "Loaded ~A" (uiop:native-namestring file)))
         (error (condition)
           (error-message luvland "Error loading ~A: ~A"
                          (uiop:native-namestring file) condition)))))))
