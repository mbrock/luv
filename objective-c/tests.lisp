(in-package #:luv.tests)

(objc:define-objective-c-message make-exception-test-array
    ("new" :object :ownership :owned :class "NSMutableArray"))

(objc:define-objective-c-message exception-test-array-object-at-index
    ("objectAtIndex:" :object :ownership :borrowed)
  (index :uint64))

(objc:define-objective-c-message exception-test-array-count
    ("count" :uint64))

(objc:define-objective-c-message malformed-exception-test-array-count
    ("count" :uint32))

(define-test objective-c-dispatch-entry-is-resolved-once
  (true (boundp 'objc::*objective-c-message-send-pointer*))
  (true (cffi:pointerp objc::*objective-c-message-send-pointer*))
  (true (not (cffi:null-pointer-p objc::*objective-c-message-send-pointer*)))
  (true (not (fboundp 'objc::objective-c-message-send-pointer))))

(define-test message-definitions-retain-selector-abi-and-ownership
  (let ((description
          (objc:objective-c-message-description 'metal:device-name)))
    (true (equal (getf description :selector) "name"))
    (true (eq (getf description :result-type) :object))
    (true (eq (getf description :result-ownership) :borrowed))
    (true (equal (getf description :result-class) "NSString"))
    (true (equal (getf description :argument-types)
                 '((objc::receiver :object))))))

(define-test classes-and-objects-preserve-native-identity-and-ownership
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (device (metal:make-system-default-device))
      (true (typep device 'objc:objective-c-object))
      (true (eq (objc:objective-c-object-ownership device) :owned))
      (true (equal (objc:objective-c-object-protocol-name device) "MTLDevice"))
      (let* ((borrowed-name (metal:device-name device))
             (owned-name (objc:retain-objective-c-object borrowed-name)))
        (true (eq (objc:objective-c-object-ownership borrowed-name) :borrowed))
        (true (objc:objective-c-object= borrowed-name owned-name))
        (fail (objc:release-objective-c-object borrowed-name)
              'objc:objective-c-ownership-error)
        (objc:release-objective-c-object owned-name)
        (true (objc:objective-c-object-released-p owned-name))
        (fail (objc:objective-c-pointer owned-name)
              'objc:released-objective-c-object)))))

(define-test declared-messages-have-opt-in-backend-local-tracing
  (let (trace)
    (objc:with-autorelease-pool ()
      (objc:with-owned-objective-c-object
          (device (metal:make-system-default-device))
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (true (plusp (metal:device-registry-id device)))
          (true (stringp
                 (objc:objective-c-string (metal:device-name device)))))))
    (let* ((events (objc:objective-c-trace-events trace))
           (descriptions
             (mapcar #'objc:objective-c-message-event-description events))
           (registry
             (find "registryID" descriptions :test #'equal
                   :key (lambda (description)
                          (getf description :selector)))))
      (true (= (length events) 3))
      (true registry)
      (true (eq (getf registry :status) :returned))
      (true (eq (getf registry :result-type) :uint64))
      (true (eq (first (second (first (getf registry :arguments))))
                :objective-c-object)))))

(define-test exception-policy-is-dynamic-and-tracing-is-orthogonal
  (true (eq objc:*objective-c-exception-policy* :unchecked))
  (let (trace)
    (objc:with-autorelease-pool ()
      (objc:with-owned-objective-c-object
          (device (metal:make-system-default-device))
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (objc:with-unchecked-objective-c-messages ()
            (true (eq objc:*objective-c-exception-policy* :unchecked))
            (true (plusp (metal:device-registry-id device)))
            (objc:with-objective-c-exception-handling ()
              (true (eq objc:*objective-c-exception-policy* :catch))
              (true (stringp
                     (objc:objective-c-string (metal:device-name device)))))
            (true (eq objc:*objective-c-exception-policy* :unchecked))))))
    (true (eq objc:*objective-c-exception-policy* :unchecked))
    (let ((events (objc:objective-c-trace-events trace)))
      (true (= (length events) 3))
      (dolist (event events)
        (true (eq (objc:objective-c-message-event-status event) :returned))))))

(define-test fresh-device-probe-is-bounded-and-printable
  (let ((description (metal:probe-system-default-device)))
    (true (stringp (getf description :class)))
    (true (equal (getf description :protocol) "MTLDevice"))
    (true (plusp (length (getf description :name))))
    (true (plusp (getf description :registry-id)))))

(define-test native-exceptions-become-conditions-and-leave-the-image-live
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
          (true (zerop (exception-test-array-count array))))
        (true (typep condition 'objc:objective-c-exception))
        (true (equal (objc:objective-c-exception-name condition)
                     "NSRangeException"))
        (true (equal (objc:objective-c-exception-selector condition)
                     "objectAtIndex:"))
        (true (eq (objc:objective-c-exception-receiver condition) array))
        (true (eq (objc:objective-c-exception-message condition)
                  'exception-test-array-object-at-index))
        (true (plusp (length (objc:objective-c-exception-reason condition))))
        (true (plusp
               (length (objc:objective-c-exception-call-stack condition))))
        (let ((events (objc:objective-c-trace-events trace)))
          (true (= (length events) 2))
          (true (eq (objc:objective-c-message-event-status (first events))
                    :signaled))
          (true (eq (getf (objc:objective-c-message-event-condition
                            (first events))
                          :type)
                    'objc:objective-c-exception))
          (true (eq (objc:objective-c-message-event-status (second events))
                    :returned)))))))

(define-test declaration-abi-mismatches-stop-before-the-message-send
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
          (true (search "8-byte result"
                        (objc:objective-c-exception-reason condition)))
          (true (not (typep condition 'objc:objective-c-exception)))))
      (true (zerop (exception-test-array-count array))))))
