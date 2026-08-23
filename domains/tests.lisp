(in-package #:luv.tests)

(deftest identity-vocabulary-offsets-are-stable-and-closed
  (let* ((stone (list :stone))
         (earth (list :earth))
         (vocabulary
           (luv.domains:make-identity-vocabulary-domain
            :members (list nil stone) :limit 3)))
    (ok (= 2 (luv.domains:domain-cardinality vocabulary)))
    (ok (= 0 (luv.domains:identity-vocabulary-offset vocabulary nil nil)))
    (ok (= 1 (luv.domains:identity-vocabulary-offset vocabulary stone nil)))
    (ok (eq stone (luv.domains:identity-vocabulary-member vocabulary 1)))
    (ok (null (luv.domains:identity-vocabulary-offset
               vocabulary (list :stone) nil)))
    (ok (zerop (luv.domains:identity-vocabulary-revision vocabulary)))
    (ok (= 2 (luv.domains:identity-vocabulary-offset vocabulary earth)))
    (ok (= 1 (luv.domains:identity-vocabulary-revision vocabulary)))
    (ok (= 1 (luv.domains:identity-vocabulary-offset vocabulary stone)))
    (ok (signals (luv.domains:identity-vocabulary-offset vocabulary (list :air))
                 'error))
    (ok (signals (luv.domains:identity-vocabulary-member vocabulary 3)
                 'error))))

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
