(defpackage #:luv/objective-c/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:objc #:luv.objective-c)
                    (#:metal #:luv.metal)))

(in-package #:luv/objective-c/tests)

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

(deftest declared-messages-compose-with-general-invocation-tracing
  (let (trace)
    (objc:with-autorelease-pool ()
      (objc:with-owned-objective-c-object
          (device (metal:make-system-default-device))
        (objc:with-objective-c-trace (active-trace)
          (setf trace active-trace)
          (ok (plusp (metal:device-registry-id device)))
          (ok (stringp
               (objc:objective-c-string (metal:device-name device)))))))
    (let* ((events (luv.invocation:invocation-trace-events trace))
           (descriptions
             (mapcar #'objc:objective-c-invocation-description events))
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

(deftest fresh-device-probe-is-bounded-and-printable
  (let ((description (metal:probe-system-default-device)))
    (ok (stringp (getf description :class)))
    (ok (equal (getf description :protocol) "MTLDevice"))
    (ok (plusp (length (getf description :name))))
    (ok (plusp (getf description :registry-id)))))
