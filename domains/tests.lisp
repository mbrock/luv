(in-package #:luv.tests)

(deftest keyword-vocabularies-close-dense-offsets-over-an-explicit-domain
  (let* ((first
           (luv.domains:make-keyword-vocabulary-domain
            '(:grass :stone :crystal)))
         (second
           (luv.domains:make-keyword-vocabulary-domain
            '(:crystal :grass :stone))))
    (ok (= 3 (luv.domains:domain-cardinality first)))
    (ok (= 1 (luv.domains:keyword-vocabulary-offset first :stone)))
    (ok (eq :stone (luv.domains:keyword-vocabulary-keyword first 1)))
    (ok (eq :grass (luv.domains:keyword-vocabulary-keyword second 1)))
    (ok (null (luv.domains:keyword-vocabulary-offset first :air nil)))
    (ok (signals (luv.domains:keyword-vocabulary-offset first :air) 'error))
    (ok (signals (luv.domains:keyword-vocabulary-keyword first 3) 'error))))

(deftest keyword-vocabularies-are-immutable-and-unambiguous
  (let* ((source (vector :dirt :sand))
         (domain
           (luv.domains:make-keyword-vocabulary-domain source))
         (members (luv.domains:keyword-vocabulary-members domain)))
    (setf (aref source 0) :clay)
    (setf (aref members 0) :mud)
    (ok (eq :dirt (luv.domains:keyword-vocabulary-keyword domain 0)))
    (ok (signals
         (luv.domains:make-keyword-vocabulary-domain '(:dirt :dirt))
         'error))
    (ok (signals
         (luv.domains:make-keyword-vocabulary-domain '(:dirt stone))
         'error))))
