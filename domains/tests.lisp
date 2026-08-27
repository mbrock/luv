(in-package #:luv.tests)

(define-test identity-vocabulary-offsets-are-stable-and-closed
  (let* ((stone (list :stone))
         (earth (list :earth))
         (vocabulary
           (luv.domains:make-identity-vocabulary-domain
            :members (list nil stone) :limit 3)))
    (true (= 2 (luv.domains:domain-cardinality vocabulary)))
    (true (= 0 (luv.domains:identity-vocabulary-offset vocabulary nil nil)))
    (true (= 1 (luv.domains:identity-vocabulary-offset vocabulary stone nil)))
    (true (eq stone (luv.domains:identity-vocabulary-member vocabulary 1)))
    (true (null (luv.domains:identity-vocabulary-offset
                 vocabulary (list :stone) nil)))
    (true (zerop (luv.domains:identity-vocabulary-revision vocabulary)))
    (true (= 2 (luv.domains:identity-vocabulary-offset vocabulary earth)))
    (true (= 1 (luv.domains:identity-vocabulary-revision vocabulary)))
    (true (= 1 (luv.domains:identity-vocabulary-offset vocabulary stone)))
    (fail (luv.domains:identity-vocabulary-offset vocabulary (list :air))
          'error)
    (fail (luv.domains:identity-vocabulary-member vocabulary 3)
          'error)))

(define-test keyword-vocabularies-close-dense-offsets-over-an-explicit-domain
  (let* ((first
           (luv.domains:make-keyword-vocabulary-domain
            '(:grass :stone :crystal)))
         (second
           (luv.domains:make-keyword-vocabulary-domain
            '(:crystal :grass :stone))))
    (true (= 3 (luv.domains:domain-cardinality first)))
    (true (= 1 (luv.domains:keyword-vocabulary-offset first :stone)))
    (true (eq :stone (luv.domains:keyword-vocabulary-keyword first 1)))
    (true (eq :grass (luv.domains:keyword-vocabulary-keyword second 1)))
    (true (null (luv.domains:keyword-vocabulary-offset first :air nil)))
    (fail (luv.domains:keyword-vocabulary-offset first :air) 'error)
    (fail (luv.domains:keyword-vocabulary-keyword first 3) 'error)))

(define-test keyword-vocabularies-are-immutable-and-unambiguous
  (let* ((source (vector :dirt :sand))
         (domain
           (luv.domains:make-keyword-vocabulary-domain source))
         (members (luv.domains:keyword-vocabulary-members domain)))
    (setf (aref source 0) :clay)
    (setf (aref members 0) :mud)
    (true (eq :dirt (luv.domains:keyword-vocabulary-keyword domain 0)))
    (fail
     (luv.domains:make-keyword-vocabulary-domain '(:dirt :dirt))
     'error)
    (fail
     (luv.domains:make-keyword-vocabulary-domain '(:dirt stone))
     'error)))
