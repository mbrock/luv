;;;; Reading api.tl.
;;;;
;;;; Telegram publishes its whole API as one TL schema: about twenty-three
;;;; hundred constructors and functions, each a line like
;;;;
;;;;   messages.sendMessage#fe05d6e4 flags:# no_webpage:flags.1?true
;;;;     peer:InputPeer message:string random_id:long = Updates;
;;;;
;;;; The tempting move is to generate a class per constructor.  It is also the
;;;; wrong one.  That set is closed and someone else owns it: we never add a
;;;; member, never attach behaviour to one, and would pay for twenty-three
;;;; hundred classes and ten thousand symbols to get nothing back but names.
;;;; The project's own rule for this is explicit -- an externally owned
;;;; enumeration is a table, not a protocol.
;;;;
;;;; So this file only reads text.  It turns a schema into the compact list
;;;; form INSTALL-TL-SCHEMA loads, and DEFINE-TL-SCHEMA does that at
;;;; macroexpansion time so the fasl carries one list literal instead of
;;;; thousands of definitions.  The records themselves live in telegram/tl.lisp,
;;;; beside the codec that reads and writes them.

(in-package #:telegram.tl)

;;;; Tokenizing the schema text


(defun strip-tl-comment (line)
  (let ((position (search "//" line)))
    (if position (subseq line 0 position) line)))

(defun split-on-semicolons (text)
  "TEXT split into nonempty declarations."
  (loop with start = 0
        for position = (position #\; text :start start)
        while position
        collect (string-trim '(#\Space #\Tab) (subseq text start position))
          into pieces
        do (setf start (1+ position))
        finally (return (remove "" pieces :test #'string=))))

(defun tl-declarations (text)
  "Split schema TEXT into (SECTION LINE DECLARATION) triples.  Section markers
switch between the type constructors and the callable functions."
  (let ((declarations '())
        (section :types)
        (start-line 1)
        (buffer ""))
    (with-input-from-string (stream text)
      (loop for line = (read-line stream nil nil)
            for number from 1
            while line
            do (let ((content (string-trim '(#\Space #\Tab #\Return)
                                           (strip-tl-comment line))))
                 (cond ((string= content "---types---") (setf section :types))
                       ((string= content "---functions---")
                        (setf section :functions))
                       ((string= content ""))
                       (t
                        (when (string= buffer "")
                          (setf start-line number))
                        (setf buffer (if (string= buffer "")
                                         content
                                         (concatenate 'string buffer " "
                                                      content)))
                        (when (find #\; content)
                          (dolist (piece (split-on-semicolons buffer))
                            (push (list section start-line piece) declarations))
                          (setf buffer "")))))))
    (nreverse declarations)))

(defun tl-tokens (declaration)
  "DECLARATION split on whitespace."
  (loop with length = (length declaration)
        with start = 0
        while (< start length)
        for end = (or (position-if (lambda (character)
                                     (member character '(#\Space #\Tab)))
                                   declaration :start start)
                      length)
        unless (= start end)
          collect (subseq declaration start end)
        do (setf start (1+ end))))

;;;; Type expressions

(defparameter +tl-primitive-specifications+
  '(("int" . int)
    ("long" . signed-long)
    ("double" . double)
    ("string" . string)
    ("bytes" . bytes)
    ("int128" . int128)
    ("int256" . int256)
    ("Bool" . bool)
    ("true" . flag)
    ("#" . flags))
  "How TL's built-in types map onto this codec's slot vocabulary.  Note that
TL's `long' is signed: Telegram's chat identifiers really are negative.")

(defun tl-vector-element (text)
  "The element type of Vector<T> or vector<T>, or NIL."
  (let ((open (position #\< text)))
    (when (and open (char= #\> (char text (1- (length text))))
               (member (subseq text 0 open) '("Vector" "vector")
                       :test #'string=))
      (subseq text (1+ open) (1- (length text))))))

(defun parse-tl-type (text &key line)
  "Parse a TL type expression into a slot specification and, as a second
value, the condition (FLAGS-NAME . BIT) guarding it."
  (let ((question (position #\? text)))
    (when question
      (let* ((guard (subseq text 0 question))
             (dot (position #\. guard)))
        (unless dot
          (error 'tl-schema-error :line line
                                  :detail (format nil "malformed condition ~S"
                                                  text)))
        (return-from parse-tl-type
          (values (parse-tl-type (subseq text (1+ question)) :line line)
                  (cons (subseq guard 0 dot)
                        (parse-integer guard :start (1+ dot))))))))
  (values
   (cond ((and (plusp (length text)) (char= #\! (char text 0))) 'object)
         ((string= text "Object") 'object)
         ((tl-vector-element text)
          (list (if (char= #\V (char text 0)) 'vector 'bare-vector)
                (parse-tl-type (tl-vector-element text) :line line)))
         ((cdr (assoc text +tl-primitive-specifications+ :test #'string=)))
         ;; Anything else names a boxed type.  The name is kept so that the
         ;; printer and DESCRIBE-TL can say what was expected, but decoding
         ;; goes by the constructor id on the wire, as TL intends.
         (t (list 'object text)))
   nil))

;;;; Parsing declarations

(defun parse-tl-field (token &key line)
  (let ((colon (position #\: token)))
    (unless colon
      (error 'tl-schema-error :line line
                              :detail (format nil "malformed field ~S" token)))
    (let ((name (subseq token 0 colon))
          (type (subseq token (1+ colon))))
      (multiple-value-bind (specification condition)
          (parse-tl-type type :line line)
        (list name specification condition)))))

(defun parse-tl-declaration (declaration &key (section :types) line)
  "Parse one DECLARATION into the compact list form the schema literal uses,
or NIL for one this reader does not model."
  (let* ((tokens (tl-tokens declaration))
         (equals (position "=" tokens :test #'string=)))
    (unless (and tokens equals)
      (return-from parse-tl-declaration nil))
    (let* ((head (first tokens))
           (hash (position #\# head))
           (name (if hash (subseq head 0 hash) head))
           (id (when hash (parse-integer head :start (1+ hash) :radix 16)))
           (result (format nil "~{~A~^ ~}" (subseq tokens (1+ equals))))
           (field-tokens (subseq tokens 1 equals)))
      ;; Built-in declarations (int ? = Int;) and the vector template carry no
      ;; constructor id, or use syntax outside this subset.  They describe the
      ;; codec rather than extend it, so they are skipped by design.
      (when (or (null id)
                (some (lambda (token)
                        (member token '("#" "[" "]" "?") :test #'string=))
                      field-tokens))
        (return-from parse-tl-declaration nil))
      (list name id (eq section :functions)
            (if (string= result "X") 'object (parse-tl-type result :line line))
            result declaration
            ;; {X:Type} declares a generic parameter, which this codec handles
            ;; by treating !X as an ordinary boxed object.
            (loop for token in field-tokens
                  unless (char= #\{ (char token 0))
                    collect (parse-tl-field token :line line))))))

(defun parse-tl-schema (text)
  "Every definition in schema TEXT, in the compact list form, with duplicate
constructor ids resolved.

Telegram's schema really does repeat four ids: the invokeWith*Prefix entries
document the header of a wrapper whose real definition appears later, so the
later one is the one to keep."
  (let ((entries (loop for (section line declaration) in (tl-declarations text)
                       for entry = (parse-tl-declaration declaration
                                                         :section section
                                                         :line line)
                       when entry collect entry))
        (seen (make-hash-table)))
    (dolist (entry entries)
      (setf (gethash (second entry) seen) entry))
    (remove-if-not (lambda (entry) (eq entry (gethash (second entry) seen)))
                   entries)))

(defun read-tl-schema-file (pathname)
  "Parse the schema at PATHNAME into the compact list form."
  (parse-tl-schema (with-open-file (stream pathname :external-format :utf-8)
                     (let ((text (make-string (file-length stream))))
                       (subseq text 0 (read-sequence text stream))))))

;;;; Installing

(defun install-tl-schema (entries)
  "Load ENTRIES -- the compact list form -- into the schema tables."
  (loop for (name id function-p result-specification result-name source fields)
          in entries
        for definition
          = (make-tl-definition
             name (tl-keyword name) id function-p result-specification
             result-name source
             (map 'simple-vector
                  (lambda (field)
                    (destructuring-bind (field-name specification condition)
                        field
                      (make-tl-field field-name (tl-keyword field-name)
                                     specification
                                     (when condition
                                       (cons (tl-keyword (car condition))
                                             (cdr condition))))))
                  fields))
        do (setf (gethash id *tl-definitions-by-id*) definition
                 (gethash (tl-definition-keyword definition)
                          *tl-definitions-by-keyword*)
                 definition)
        count t))

(defmacro define-tl-schema (pathname)
  "Read the TL schema at PATHNAME -- relative to the file being compiled --
and expand into a single quoted table that INSTALL-TL-SCHEMA loads.

The schema text stays the one authority; the fasl carries a list literal
rather than thousands of definitions, so compiling is quick and loading is
quicker."
  (let ((source (merge-pathnames pathname
                                 (or *compile-file-truename* *load-truename*
                                     *default-pathname-defaults*))))
    `(install-tl-schema ',(read-tl-schema-file source))))
