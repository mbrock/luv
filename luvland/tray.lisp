;;;; Tray icons: Luvland is the StatusNotifierWatcher and its only host.
;;;;
;;;; Tray icons on a Linux desktop are not a Wayland protocol.  Programs such
;;;; as nm-applet and blueman publish a StatusNotifierItem on the session bus
;;;; and register it with whoever owns org.kde.StatusNotifierWatcher; a bar
;;;; then reads each item's title, icon, and menu over D-Bus.  Luvland owns
;;;; the watcher name itself, so an item registers straight into the bar.
;;;;
;;;; Everything here runs on the bus's dispatch thread, which may call items
;;;; synchronously.  The bar reads only TRAY-ITEMS, an immutable list
;;;; published after each change.  An item's menu (com.canonical.dbusmenu)
;;;; is read ahead into a flat list, so the play menu and a click can offer
;;;; it without waiting on the item.

(in-package #:luvland)

(defparameter +watcher-path+ "/StatusNotifierWatcher")
(defparameter +watcher-names+ '("org.kde.StatusNotifierWatcher"
                                "org.freedesktop.StatusNotifierWatcher"))
(defparameter +item-interfaces+ '("org.kde.StatusNotifierItem"
                                  "org.freedesktop.StatusNotifierItem"))
(defparameter +dbusmenu+ "com.canonical.dbusmenu")

(defparameter *tray-icon-size* 48
  "The pixmap size, in pixels, the tray prefers among those an item offers.")

(defparameter *tray-menu-limit* 48
  "At most this many entries are read from one item's menu.")

(defstruct tray-item
  "One StatusNotifierItem as the bar shows it."
  key          ; "service/path", as the watcher names it
  service      ; the name it registered under
  owner        ; that name's unique owner, the sender of its signals
  path
  interface
  id title status icon-name
  pixmap       ; (width height octets), octets ARGB32 big-endian, or NIL
  item-is-menu-p
  menu-path
  menu)        ; ((label . id) ...), submenus flattened as "Parent › Child"

(defclass tray ()
  ((bus :initarg :bus :reader tray-bus)
   (daemon :initarg :daemon :initform nil :reader tray-daemon
           :documentation "The dbus-daemon Luvland started, when there was no session bus.")
   (lock :initform (sb-thread:make-mutex :name "Luvland tray") :reader tray-lock)
   (items :initform '() :accessor tray-items
          :documentation "TRAY-ITEMs in registration order; replaced, never mutated.")
   (watcher-p :initform nil :accessor tray-watcher-p
              :documentation "Whether Luvland owns the watcher name.")))

(defmethod print-object ((tray tray) stream)
  (print-unreadable-object (tray stream :type t)
    (format stream "~D item~:P~:[ (host only)~;~]" (length (tray-items tray))
            (tray-watcher-p tray))))

;;; The published list.

(defun publish-item (tray item)
  (sb-thread:with-mutex ((tray-lock tray))
    (let ((items (tray-items tray)))
      (setf (tray-items tray)
            (if (find (tray-item-key item) items :key #'tray-item-key :test #'string=)
                (substitute-if item (lambda (old) (string= (tray-item-key old)
                                                           (tray-item-key item)))
                               items)
                (append items (list item)))))))

(defun forget-items (tray predicate)
  "Remove the items PREDICATE accepts; return them."
  (sb-thread:with-mutex ((tray-lock tray))
    (let ((gone (remove-if-not predicate (tray-items tray))))
      (setf (tray-items tray) (remove-if predicate (tray-items tray)))
      gone)))

;;; Reading an item.

(defun best-pixmap (pixmaps)
  "Of PIXMAPS, each (width height octets), the smallest at least
*TRAY-ICON-SIZE* wide, else the largest."
  (let ((valid (remove-if-not (lambda (pixmap)
                                (destructuring-bind (width height octets) pixmap
                                  (and (plusp width) (plusp height)
                                       (= (length octets) (* 4 width height)))))
                              pixmaps)))
    (or (first (sort (remove-if (lambda (pixmap) (< (first pixmap) *tray-icon-size*))
                                (copy-list valid))
                     #'< :key #'first))
        (first (sort (copy-list valid) #'> :key #'first)))))

(defun property (name properties)
  (cdr (assoc name properties :test #'string=)))

(defun menu-label (label)
  "LABEL without its mnemonic underscores: \"_Foo\" is Foo, \"__\" is _."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length label))
          do (let ((char (char label i)))
               (cond ((and (char= char #\_) (< (1+ i) (length label))
                           (char= (char label (1+ i)) #\_))
                      (write-char #\_ out) (incf i 2))
                     ((char= char #\_) (incf i))
                     (t (write-char char out) (incf i)))))))

(defun property-false-p (name properties)
  "Whether PROPERTIES say NAME is false; absent means its default, true."
  (let ((entry (assoc name properties :test #'string=)))
    (and entry (null (cdr entry)))))

(defun flatten-menu (layout &optional (prefix nil))
  "The entries of dbusmenu LAYOUT, (id properties children), that can be
clicked, as (label . id), submenus' entries prefixed with their own labels."
  (loop for child in (third layout)
        for (id properties) = child
        for label = (menu-label (or (property "label" properties) ""))
        for full = (if prefix (format nil "~A › ~A" prefix label) label)
        unless (or (property-false-p "visible" properties)
                   (equal (property "type" properties) "separator")
                   (string= label ""))
          append (if (equal (property "children-display" properties) "submenu")
                     (flatten-menu child full)
                     (unless (property-false-p "enabled" properties)
                       (list (cons (let ((toggle (property "toggle-type" properties)))
                                     (if (member toggle '("checkmark" "radio") :test #'equal)
                                         (format nil "~:[○~;●~] ~A"
                                                 (eql 1 (property "toggle-state" properties))
                                                 full)
                                         full))
                                   id))))))

(defun read-menu (bus owner path)
  (when (and path (string/= path "/"))
    (handler-case
        (progn
          ;; Some items fill their menu only when told it is about to show.
          (ignore-errors (luv.dbus:call bus owner path +dbusmenu+ "AboutToShow"
                                        :signature "i" :arguments '(0) :timeout 2))
          (let ((layout (second (luv.dbus:call bus owner path +dbusmenu+ "GetLayout"
                                               :signature "iias" :arguments '(0 -1 ())
                                               :timeout 3))))
            (let ((entries (flatten-menu layout)))
              (subseq entries 0 (min (length entries) *tray-menu-limit*)))))
      (error () nil))))

(defun read-item (tray service path &key key)
  "Read the item SERVICE serves at PATH, or NIL if it does not answer."
  (let ((bus (tray-bus tray)))
    (handler-case
        (let ((owner (if (char= (char service 0) #\:)
                         service
                         (first (luv.dbus:call bus "org.freedesktop.DBus" "/org/freedesktop/DBus"
                                               "org.freedesktop.DBus" "GetNameOwner"
                                               :signature "s" :arguments (list service))))))
          (loop for interface in +item-interfaces+
                for properties = (ignore-errors
                                  (luv.dbus:get-all-properties bus owner path interface))
                when properties
                  return (let ((menu-path (property "Menu" properties)))
                           (make-tray-item
                            :key (or key (format nil "~A~A" service path))
                            :service service :owner owner :path path :interface interface
                            :id (property "Id" properties)
                            :title (property "Title" properties)
                            :status (property "Status" properties)
                            :icon-name (if (equal (property "Status" properties) "NeedsAttention")
                                           (or (property "AttentionIconName" properties)
                                               (property "IconName" properties))
                                           (property "IconName" properties))
                            :pixmap (best-pixmap
                                     (if (equal (property "Status" properties) "NeedsAttention")
                                         (or (property "AttentionIconPixmap" properties)
                                             (property "IconPixmap" properties))
                                         (property "IconPixmap" properties)))
                            :item-is-menu-p (property "ItemIsMenu" properties)
                            :menu-path menu-path
                            :menu (read-menu bus owner menu-path)))))
      (error () nil))))

(defun refresh-item (tray item)
  (let ((fresh (read-item tray (tray-item-service item) (tray-item-path item)
                          :key (tray-item-key item))))
    (if fresh
        (publish-item tray fresh)
        (forget-items tray (lambda (old) (eq old item))))))

;;; The watcher.

(defun register-item (tray sender argument)
  "Register the item ARGUMENT names: a bus name, served at
/StatusNotifierItem, or an object path SENDER serves, as libappindicator
registers."
  (multiple-value-bind (service path)
      (if (and (plusp (length argument)) (char= (char argument 0) #\/))
          (values sender argument)
          (values argument "/StatusNotifierItem"))
    (let ((item (read-item tray service path)))
      (when item
        (publish-item tray item)
        (when (tray-watcher-p tray)
          (luv.dbus:emit-signal (tray-bus tray) +watcher-path+ (first +watcher-names+)
                                "StatusNotifierItemRegistered"
                                :signature "s" :arguments (list (tray-item-key item))))))))

(defun item-forgotten (tray item)
  (when (tray-watcher-p tray)
    (ignore-errors
     (luv.dbus:emit-signal (tray-bus tray) +watcher-path+ (first +watcher-names+)
                           "StatusNotifierItemUnregistered"
                           :signature "s" :arguments (list (tray-item-key item))))))

(defparameter +watcher-introspection+
  "<!DOCTYPE node PUBLIC \"-//freedesktop//DTD D-BUS Object Introspection 1.0//EN\"
 \"http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd\">
<node>
  <interface name=\"org.kde.StatusNotifierWatcher\">
    <method name=\"RegisterStatusNotifierItem\"><arg name=\"service\" type=\"s\" direction=\"in\"/></method>
    <method name=\"RegisterStatusNotifierHost\"><arg name=\"service\" type=\"s\" direction=\"in\"/></method>
    <property name=\"RegisteredStatusNotifierItems\" type=\"as\" access=\"read\"/>
    <property name=\"IsStatusNotifierHostRegistered\" type=\"b\" access=\"read\"/>
    <property name=\"ProtocolVersion\" type=\"i\" access=\"read\"/>
    <signal name=\"StatusNotifierItemRegistered\"><arg type=\"s\"/></signal>
    <signal name=\"StatusNotifierItemUnregistered\"><arg type=\"s\"/></signal>
    <signal name=\"StatusNotifierHostRegistered\"/>
  </interface>
  <interface name=\"org.freedesktop.DBus.Properties\">
    <method name=\"Get\"><arg type=\"s\" direction=\"in\"/><arg type=\"s\" direction=\"in\"/><arg type=\"v\" direction=\"out\"/></method>
    <method name=\"GetAll\"><arg type=\"s\" direction=\"in\"/><arg type=\"a{sv}\" direction=\"out\"/></method>
  </interface>
  <interface name=\"org.freedesktop.DBus.Introspectable\">
    <method name=\"Introspect\"><arg type=\"s\" direction=\"out\"/></method>
  </interface>
</node>")

(defun watcher-properties (tray)
  (list (cons "RegisteredStatusNotifierItems"
              (luv.dbus:make-variant "as" (mapcar #'tray-item-key (tray-items tray))))
        (cons "IsStatusNotifierHostRegistered" (luv.dbus:make-variant "b" t))
        (cons "ProtocolVersion" (luv.dbus:make-variant "i" 0))))

(defun handle-watcher-call (tray bus message)
  (let ((member (luv.dbus:message-member message))
        (arguments (luv.dbus:message-body message)))
    (cond
      ((string= member "RegisterStatusNotifierItem")
       (luv.dbus:reply bus message)
       (register-item tray (luv.dbus:message-sender message) (first arguments)))
      ((string= member "RegisterStatusNotifierHost")
       (luv.dbus:reply bus message)
       (luv.dbus:emit-signal bus +watcher-path+ (first +watcher-names+)
                             "StatusNotifierHostRegistered"))
      ((string= member "GetAll")
       (luv.dbus:reply bus message :signature "a{sv}" :arguments (list (watcher-properties tray))))
      ((string= member "Get")
       (let ((entry (assoc (second arguments) (watcher-properties tray) :test #'string=)))
         (if entry
             (luv.dbus:reply bus message :signature "v" :arguments (list (cdr entry)))
             (luv.dbus:reply-error bus message "org.freedesktop.DBus.Error.UnknownProperty"
                                   (second arguments)))))
      ((string= member "Introspect")
       (luv.dbus:reply bus message :signature "s" :arguments (list +watcher-introspection+)))
      ((string= member "Ping")
       (luv.dbus:reply bus message))
      (t (return-from handle-watcher-call nil)))
    t))

(defun handle-tray-signal (tray message)
  (let ((interface (luv.dbus:message-interface message))
        (member (luv.dbus:message-member message))
        (sender (luv.dbus:message-sender message)))
    (cond
      ((and (equal interface "org.freedesktop.DBus") (equal member "NameOwnerChanged"))
       (destructuring-bind (name old new) (luv.dbus:message-body message)
         (declare (ignore old))
         (when (string= new "")
           (dolist (item (forget-items tray (lambda (item)
                                              (or (string= (tray-item-owner item) name)
                                                  (string= (tray-item-service item) name)))))
             (item-forgotten tray item)))))
      ((member interface +item-interfaces+ :test #'equal)
       (dolist (item (tray-items tray))
         (when (and (string= (tray-item-owner item) sender)
                    (string= (tray-item-path item) (luv.dbus:message-path message)))
           (refresh-item tray item))))
      ((equal interface +dbusmenu+)
       (when (member member '("LayoutUpdated" "ItemsPropertiesUpdated") :test #'string=)
         (dolist (item (tray-items tray))
           (when (and (string= (tray-item-owner item) sender)
                      (equal (tray-item-menu-path item) (luv.dbus:message-path message)))
             (publish-item tray (let ((copy (copy-tray-item item)))
                                  (setf (tray-item-menu copy)
                                        (read-menu (tray-bus tray) sender
                                                   (tray-item-menu-path item)))
                                  copy))))))
      ;; Host only: another program is the watcher.
      ((and (member interface +watcher-names+ :test #'equal)
            (equal member "StatusNotifierItemRegistered"))
       (let* ((key (first (luv.dbus:message-body message)))
              (slash (position #\/ key)))
         (register-item tray (subseq key 0 (or slash (length key)))
                        (if slash (subseq key slash) (subseq key 0))))))))

(defun tray-handler (tray)
  (lambda (bus message)
    (case (luv.dbus:message-type message)
      (:method-call
       (and (equal (luv.dbus:message-path message) +watcher-path+)
            (tray-watcher-p tray)
            (handle-watcher-call tray bus message)))
      (:signal
       (handle-tray-signal tray message)
       nil))))

;;; Starting and stopping.

(defun start-session-bus ()
  "Start a dbus-daemon for Luvland's clients, as a session on a console
would have had one; return the process, or NIL."
  (ignore-errors
   (let* ((process (sb-ext:run-program "dbus-daemon"
                                       '("--session" "--nofork" "--print-address=1")
                                       :search t :wait nil :output :stream :error nil))
          (address (read-line (sb-ext:process-output process) nil nil)))
     (if (and address (plusp (length address)))
         (progn (sb-posix:setenv "DBUS_SESSION_BUS_ADDRESS" (string-trim " " address) 1)
                process)
         (progn (ignore-errors (sb-ext:process-kill process 15)) nil)))))

(defun start-tray ()
  "Connect to the session bus, starting one if there is none, and become
the StatusNotifierWatcher; return the TRAY, or NIL without a bus."
  (let* ((daemon (unless (luv.dbus:session-bus-address) (start-session-bus)))
         (bus (ignore-errors (luv.dbus:open-bus :session))))
    (cond
      ((null bus)
       (when daemon (ignore-errors (sb-ext:process-kill daemon 15)))
       nil)
      (t
       (let ((tray (make-instance 'tray :bus bus :daemon daemon)))
         (luv.dbus:add-handler bus (tray-handler tray))
         (dolist (rule '("type='signal',sender='org.freedesktop.DBus',member='NameOwnerChanged'"
                         "type='signal',interface='org.kde.StatusNotifierItem'"
                         "type='signal',interface='org.freedesktop.StatusNotifierItem'"
                         "type='signal',interface='com.canonical.dbusmenu'"
                         "type='signal',interface='org.kde.StatusNotifierWatcher'"))
           (ignore-errors (luv.dbus:add-match bus rule)))
         (ignore-errors
          (luv.dbus:request-name bus (format nil "org.kde.StatusNotifierHost-~D"
                                             (sb-posix:getpid))))
         (setf (tray-watcher-p tray)
               (eq :primary-owner (ignore-errors
                                   (luv.dbus:request-name bus (first +watcher-names+)))))
         (cond
           ((tray-watcher-p tray)
            (ignore-errors (luv.dbus:request-name bus (second +watcher-names+)))
            (luv.dbus:emit-signal bus +watcher-path+ (first +watcher-names+)
                                  "StatusNotifierHostRegistered"))
           (t
            ;; Someone else watches: be one more host and read what it has.
            (ignore-errors
             (luv.dbus:call bus (first +watcher-names+) +watcher-path+ (first +watcher-names+)
                            "RegisterStatusNotifierHost" :signature "s"
                            :arguments (list (luv.dbus:bus-unique-name bus))))
            (sb-thread:make-thread
             (lambda ()
               (dolist (key (ignore-errors
                             (luv.dbus:get-property bus (first +watcher-names+) +watcher-path+
                                                    (first +watcher-names+)
                                                    "RegisteredStatusNotifierItems")))
                 (let ((slash (position #\/ key)))
                   (register-item tray (subseq key 0 (or slash (length key)))
                                  (if slash (subseq key slash) (subseq key 0))))))
             :name "Luvland tray items")))
         tray)))))

(defun stop-tray (tray)
  (when tray
    (ignore-errors (luv.dbus:close-bus (tray-bus tray)))
    (let ((daemon (tray-daemon tray)))
      (when (and daemon (sb-ext:process-alive-p daemon))
        (sb-ext:process-kill daemon 15)))))

;;; Acting on an item, from any thread; calls run on their own thread so a
;;; slow item never holds a frame.

(defun tray-call (tray item member signature arguments)
  (sb-thread:make-thread
   (lambda ()
     (ignore-errors
      (luv.dbus:call (tray-bus tray) (tray-item-owner item) (tray-item-path item)
                     (tray-item-interface item) member
                     :signature signature :arguments arguments :timeout 5)))
   :name "Luvland tray call"))

(defun activate-tray-item (tray item &key secondary-p)
  (tray-call tray item (if secondary-p "SecondaryActivate" "Activate") "ii" '(0 0)))

(defun click-tray-menu-entry (tray item id)
  (sb-thread:make-thread
   (lambda ()
     (ignore-errors
      (luv.dbus:call (tray-bus tray) (tray-item-owner item) (tray-item-menu-path item)
                     +dbusmenu+ "Event" :signature "isvu"
                     :arguments (list id "clicked" (luv.dbus:make-variant "i" 0) 0)
                     :timeout 5)))
   :name "Luvland tray menu"))

(defun tray-item-name (item)
  "What to call ITEM in the bar and the menus."
  (let ((title (tray-item-title item)))
    (if (and title (plusp (length title)))
        title
        (or (tray-item-id item) (tray-item-service item)))))
