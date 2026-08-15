(defpackage #:luv/objective-c/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:objc #:luv.objective-c)
                    (#:metal #:luv.metal)))

(in-package #:luv/objective-c/tests)

(objc:define-objective-c-message make-exception-test-array
    ("new" :object :ownership :owned :class "NSMutableArray"))

(objc:define-objective-c-message exception-test-array-object-at-index
    ("objectAtIndex:" :object :ownership :borrowed)
  (index :uint64))

(objc:define-objective-c-message exception-test-array-count
    ("count" :uint64))

(objc:define-objective-c-message malformed-exception-test-array-count
    ("count" :uint32))

(deftest objective-c-dispatch-entry-is-resolved-once
  (ok (boundp 'objc::*objective-c-message-send-pointer*))
  (ok (cffi:pointerp objc::*objective-c-message-send-pointer*))
  (ok (not (cffi:null-pointer-p objc::*objective-c-message-send-pointer*)))
  (ok (not (fboundp 'objc::objective-c-message-send-pointer))))

(deftest message-definitions-retain-selector-abi-and-ownership
  (let ((description
          (objc:objective-c-message-description 'metal:device-name)))
    (ok (equal (getf description :selector) "name"))
    (ok (eq (getf description :result-type) :object))
    (ok (eq (getf description :result-ownership) :borrowed))
    (ok (equal (getf description :result-class) "NSString"))
    (ok (equal (getf description :argument-types)
               '((objc::receiver :object))))))

(deftest classes-and-objects-preserve-native-identity-and-ownership
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (device (metal:make-system-default-device))
      (ok (typep device 'objc:objective-c-object))
      (ok (eq (objc:objective-c-object-ownership device) :owned))
      (ok (equal (objc:objective-c-object-protocol-name device) "MTLDevice"))
      (let* ((borrowed-name (metal:device-name device))
             (owned-name (objc:retain-objective-c-object borrowed-name)))
        (ok (eq (objc:objective-c-object-ownership borrowed-name) :borrowed))
        (ok (objc:objective-c-object= borrowed-name owned-name))
        (ok (signals (objc:release-objective-c-object borrowed-name)
                     'objc:objective-c-ownership-error))
        (objc:release-objective-c-object owned-name)
        (ok (objc:objective-c-object-released-p owned-name))
        (ok (signals (objc:objective-c-pointer owned-name)
                     'objc:released-objective-c-object))))))

(deftest declared-messages-have-opt-in-backend-local-tracing
  (let (trace)
    (objc:with-autorelease-pool ()
      (objc:with-owned-objective-c-object
          (device (metal:make-system-default-device))
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (ok (plusp (metal:device-registry-id device)))
          (ok (stringp
               (objc:objective-c-string (metal:device-name device)))))))
    (let* ((events (objc:objective-c-trace-events trace))
           (descriptions
             (mapcar #'objc:objective-c-message-event-description events))
           (registry
             (find "registryID" descriptions :test #'equal
                   :key (lambda (description)
                          (getf description :selector)))))
      (ok (= (length events) 3))
      (ok registry)
      (ok (eq (getf registry :status) :returned))
      (ok (eq (getf registry :result-type) :uint64))
      (ok (eq (first (second (first (getf registry :arguments))))
              :objective-c-object)))))

(deftest exception-policy-is-dynamic-and-tracing-is-orthogonal
  (ok (eq objc:*objective-c-exception-policy* :unchecked))
  (let (trace)
    (objc:with-autorelease-pool ()
      (objc:with-owned-objective-c-object
          (device (metal:make-system-default-device))
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (objc:with-unchecked-objective-c-messages ()
            (ok (eq objc:*objective-c-exception-policy* :unchecked))
            (ok (plusp (metal:device-registry-id device)))
            (objc:with-objective-c-exception-handling ()
              (ok (eq objc:*objective-c-exception-policy* :catch))
              (ok (stringp
                   (objc:objective-c-string (metal:device-name device)))))
            (ok (eq objc:*objective-c-exception-policy* :unchecked))))))
    (ok (eq objc:*objective-c-exception-policy* :unchecked))
    (let ((events (objc:objective-c-trace-events trace)))
      (ok (= (length events) 3))
      (dolist (event events)
        (ok (eq (objc:objective-c-message-event-status event) :returned))))))

(deftest fresh-device-probe-is-bounded-and-printable
  (let ((description (metal:probe-system-default-device)))
    (ok (stringp (getf description :class)))
    (ok (equal (getf description :protocol) "MTLDevice"))
    (ok (plusp (length (getf description :name))))
    (ok (plusp (getf description :registry-id)))))

(deftest native-exceptions-become-conditions-and-leave-the-image-live
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (array
          (make-exception-test-array
           (objc:find-objective-c-class "NSMutableArray")))
      (let (condition trace)
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (handler-case
              (objc:with-objective-c-exception-handling ()
                (exception-test-array-object-at-index array 0))
            (objc:objective-c-exception (signaled)
              (setf condition signaled)))
          (ok (zerop (exception-test-array-count array))))
        (ok (typep condition 'objc:objective-c-exception))
        (ok (equal (objc:objective-c-exception-name condition)
                   "NSRangeException"))
        (ok (equal (objc:objective-c-exception-selector condition)
                   "objectAtIndex:"))
        (ok (eq (objc:objective-c-exception-receiver condition) array))
        (ok (eq (objc:objective-c-exception-message condition)
                'exception-test-array-object-at-index))
        (ok (plusp (length (objc:objective-c-exception-reason condition))))
        (ok (plusp
             (length (objc:objective-c-exception-call-stack condition))))
        (let ((events (objc:objective-c-trace-events trace)))
          (ok (= (length events) 2))
          (ok (eq (objc:objective-c-message-event-status (first events))
                  :signaled))
          (ok (eq (getf (objc:objective-c-message-event-condition
                          (first events))
                        :type)
                  'objc:objective-c-exception))
          (ok (eq (objc:objective-c-message-event-status (second events))
                  :returned)))))))

(deftest declaration-abi-mismatches-stop-before-the-message-send
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (array
          (make-exception-test-array
           (objc:find-objective-c-class "NSMutableArray")))
      (handler-case
          (objc:with-objective-c-exception-handling ()
            (malformed-exception-test-array-count array)
            (fail "The malformed declaration returned."))
        (objc:objective-c-bridge-error (condition)
          (ok (search "8-byte result"
                      (objc:objective-c-exception-reason condition)))
          (ok (not (typep condition 'objc:objective-c-exception)))))
      (ok (zerop (exception-test-array-count array))))))
