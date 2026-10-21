(in-package #:luvcraft.agent)

;;; Reading the image the agent lives in.
;;;
;;; ./sly describe, apropos, xref, and edit are how a person finds their way
;;; around this code without reading all of it.  These are the same four
;;; questions as agent tools, asked of the running image directly through
;;; SB-INTROSPECT rather than through a Slynk connection, so they work in a
;;; standalone build too.  Answers name files relative to the checkout, with
;;; line numbers and the definition's own source text, so an agent that wants
;;; to change something can say exactly where it is.

(defparameter *introspection-definition-types*
  '(:macro :function :generic-function :variable :constant :class
    :condition :structure :type :method-combination)
  "The SB-INTROSPECT definition types a symbol's sources are looked up under.
Methods are left out: a generic function with forty methods would drown its
own definition.  XREF SPECIALIZES finds those.")

(defparameter *introspection-result-limit* 40
  "The most matches any one introspection answer lists.")

(defparameter *introspection-source-lines* 80
  "The most lines of one definition's source a SOURCE answer includes.")

(defun introspection-root ()
  "The checkout directory that answers name files relative to."
  (asdf:system-source-directory "luvcraft"))

(defun qualified-name (symbol)
  "SYMBOL written with its home package, in lower case."
  (let ((*package* (find-package '#:keyword)))
    (string-downcase (prin1-to-string symbol))))

(defun symbol-defined-p (symbol)
  (or (fboundp symbol) (boundp symbol) (find-class symbol nil)
      (and (fboundp symbol) (macro-function symbol))))

(defun resolve-introspection-symbols (name)
  "Every interned symbol NAME could mean that has a definition.

\"pkg:sym\" and \"pkg::sym\" name one package; a bare name is looked for in
LUVCRAFT first and then in every package, since an agent asking about
MESH-CHUNK should not need to know it lives in LUFT."
  (let* ((name (string-trim " " name))
         (colon (position #\: name)))
    (if colon
        (let* ((package (find-package (string-upcase (subseq name 0 colon))))
               (symbol-name (string-upcase
                             (string-left-trim ":" (subseq name colon)))))
          (alexandria:when-let
              ((symbol (and package (find-symbol symbol-name package))))
            (list symbol)))
        (multiple-value-bind (home status)
            (find-symbol (string-upcase name) '#:luvcraft)
          (remove-duplicates
           (remove-if-not #'symbol-defined-p
                          (append (and status (list home))
                                  (find-all-symbols (string-upcase name))))
           :from-end t)))))

(defun form-start (text)
  "Skip the blank lines and ; comments that lead TEXT up to its first form.

SBCL's recorded offset is where the reader began, just after the previous
form, so what precedes a definition belongs to the gap, not to it."
  (let ((index 0) (length (length text)))
    (loop
      (loop while (and (< index length)
                       (member (char text index)
                               '(#\Space #\Tab #\Newline #\Return #\Page)))
            do (incf index))
      (if (and (< index length) (char= (char text index) #\;))
          (setf index (or (position #\Newline text :start index) length))
          (return index)))))

(defun file-octets (pathname)
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (let ((octets (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      octets)))

(defclass source-place ()
  ((name :initarg :name :reader source-place-name)
   (file :initarg :file :reader source-place-file)
   (line :initarg :line :reader source-place-line)
   (text :initarg :text :reader source-place-text))
  (:documentation "Where one definition is written, and what is written there."))

(defun definition-source-place (name source)
  "Locate SOURCE, an SB-INTROSPECT definition source, as a SOURCE-PLACE.

SBCL records the offset as the compiling stream's file position, which is a
count of octets, not characters: a source with an em dash in it would put a
character offset part-way into the wrong form.  So the file is read as UTF-8
octets and decoded from the offset on.  The form's text is found by reading
from there with the reader suppressed, so it ends where the Lisp reader would
say the form ends."
  (let ((pathname (sb-introspect:definition-source-pathname source))
        (offset (sb-introspect:definition-source-character-offset source)))
    (when (and pathname offset (probe-file pathname))
      (let* ((octets (file-octets pathname))
             (offset (min offset (length octets)))
             (tail (sb-ext:octets-to-string octets :external-format :utf-8
                                                   :start offset))
             (start (form-start tail))
             (text (subseq tail start))
             (end (or (ignore-errors
                       (let ((*read-suppress* t))
                         (nth-value 1 (read-from-string text))))
                      (length text))))
        (make-instance
         'source-place
         :name name
         :file (enough-namestring (truename pathname) (introspection-root))
         :line (+ 1 (count 10 octets :end offset)
                  (count #\Newline tail :end start))
         :text (subseq text 0 end))))))

(defun symbol-source-places (symbol)
  (loop for type in *introspection-definition-types*
        nconc (loop for source in (ignore-errors
                                   (sb-introspect:find-definition-sources-by-name
                                    symbol type))
                    for place = (definition-source-place
                                 (format nil "~(~A~) ~A" type
                                         (qualified-name symbol))
                                 source)
                    when place collect place)))

(defun definition-name-string (name)
  "NAME, perhaps a method's (FAST-METHOD ...) list, on one lower-case line."
  (let ((*print-pretty* nil))
    (format nil "~(~S~)" name)))

(defun first-line (text)
  (let ((text (string-trim '(#\Space #\Newline #\Tab) (or text ""))))
    (subseq text 0 (position #\Newline text))))

(defun write-source-place-header (place stream)
  (format stream "~A~%  ~A:~D~%" (source-place-name place)
          (source-place-file place) (source-place-line place)))

(defun clipped-lines (text limit)
  (let ((lines (uiop:split-string text :separator '(#\Newline))))
    (if (> (length lines) limit)
        (format nil "~{~A~%~}... (~D more lines)"
                (subseq lines 0 limit) (- (length lines) limit))
        text)))

(defun symbol-kinds (symbol)
  (remove nil
          (list (cond ((macro-function symbol) "macro")
                      ((and (fboundp symbol)
                            (typep (fdefinition symbol) 'generic-function))
                       "generic function")
                      ((fboundp symbol) "function"))
                (and (boundp symbol)
                     (if (constantp symbol) "constant" "variable"))
                (and (find-class symbol nil) "class"))))

(defun no-symbol-named (name)
  (format nil "Nothing defined is named ~A.  Try apropos with part of it."
          name))

;;; ---------------------------------------------------------------------
;;; The commands.

(defmethod command-tool-runs-on-canvas-p ((command (eql 'com-describe-symbol)))
  nil)
(defmethod command-tool-runs-on-canvas-p ((command (eql 'com-apropos)))
  nil)
(defmethod command-tool-runs-on-canvas-p ((command (eql 'com-xref)))
  nil)
(defmethod command-tool-runs-on-canvas-p ((command (eql 'com-source)))
  nil)

(defmethod command-output-line-limit ((command (eql 'com-describe-symbol)))
  200)
(defmethod command-output-line-limit ((command (eql 'com-apropos)))
  200)
(defmethod command-output-line-limit ((command (eql 'com-xref)))
  200)
(defmethod command-output-line-limit ((command (eql 'com-source)))
  400)

(define-command (com-describe-symbol :command-table luvcraft-agent
                                     :name "Describe Symbol")
    ((name 'string :prompt "symbol"))
  "Describe a Lisp name in the running game: what it is, its lambda list, its documentation, its current value if a variable, and the file and line it is defined at.  NAME is like \"place-block\" or \"luvcraft:block-kind\"; a bare name is found in any package."
  (let ((symbols (resolve-introspection-symbols name)))
    (if (null symbols)
        (no-symbol-named name)
        (with-output-to-string (stream)
          (dolist (symbol symbols)
            (format stream "~A: ~{~A~^, ~}~%" (qualified-name symbol)
                    (symbol-kinds symbol))
            (when (fboundp symbol)
              (format stream "  Lambda list: ~(~A~)~%"
                      (let ((*package* (or (symbol-package symbol) *package*)))
                        (prin1-to-string
                         (sb-introspect:function-lambda-list symbol))))
              (alexandria:when-let ((doc (documentation symbol 'function)))
                (format stream "  ~A~%" doc)))
            (when (boundp symbol)
              (format stream "  Value: ~A~%"
                      (let ((*print-length* 8) (*print-level* 3))
                        (first-line (prin1-to-string
                                     (symbol-value symbol)))))
              (alexandria:when-let ((doc (documentation symbol 'variable)))
                (format stream "  ~A~%" doc)))
            (alexandria:when-let ((class (find-class symbol nil)))
              (alexandria:when-let ((doc (documentation symbol 'type)))
                (format stream "  ~A~%" doc))
              (ignore-errors
               (closer-mop:ensure-finalized class)
               (let ((slots (mapcar #'closer-mop:slot-definition-name
                                    (closer-mop:class-slots class))))
                 (when slots
                   (format stream "  Slots: ~(~{~A~^ ~}~)~%" slots)))))
            (dolist (place (symbol-source-places symbol))
              (format stream "  Defined: ~A:~D~%" (source-place-file place)
                      (source-place-line place)))
            (terpri stream))))))

(define-command (com-apropos :command-table luvcraft-agent
                             :name "Apropos")
    ((pattern 'string :prompt "part of a name"))
  "Find defined Lisp names containing PATTERN (case-insensitive) in the game's own packages, with what each is and the first line of its documentation.  Use it to discover what exists before describing or reading it."
  (let* ((pattern (string-upcase (string-trim " " pattern)))
         (symbols
           (sort (remove-duplicates
                  (remove-if-not
                   (lambda (symbol)
                     (and (symbol-defined-p symbol)
                          (let ((package (symbol-package symbol)))
                            (and package
                                 (eq package
                                     (symbol-package
                                      (find-symbol (symbol-name symbol)
                                                   package)))
                                 (not (member (package-name package)
                                              '("COMMON-LISP" "KEYWORD")
                                              :test #'string=))
                                 (not (search "SB-" (package-name package)
                                              :end2 (min 3 (length (package-name package)))))))))
                   (apropos-list pattern)))
                 #'string< :key #'qualified-name))
         (shown (subseq symbols 0 (min (length symbols)
                                       *introspection-result-limit*))))
    (if (null symbols)
        (format nil "No defined name contains ~A." pattern)
        (with-output-to-string (stream)
          (dolist (symbol shown)
            (format stream "~A (~{~A~^, ~})~@[ -- ~A~]~%"
                    (qualified-name symbol) (symbol-kinds symbol)
                    (alexandria:when-let
                        ((doc (or (documentation symbol 'function)
                                  (documentation symbol 'variable)
                                  (documentation symbol 'type))))
                      (first-line doc))))
          (when (> (length symbols) (length shown))
            (format stream "... and ~D more; narrow the pattern.~%"
                    (- (length symbols) (length shown))))))))

(defparameter *xref-relations*
  '(("callers" . sb-introspect:who-calls)
    ("references" . sb-introspect:who-references)
    ("binds" . sb-introspect:who-binds)
    ("sets" . sb-introspect:who-sets)
    ("macroexpands" . sb-introspect:who-macroexpands)
    ("specializes" . sb-introspect:who-specializes-directly)
    ("callees" . :callees))
  "The cross-references XREF answers, by the word the agent asks with.")

(defun xref-places (relation symbol)
  (if (eq relation :callees)
      (when (fboundp symbol)
        (loop for callee in (sb-introspect:find-function-callees
                             (fdefinition symbol))
              for name = (sb-kernel:%fun-name callee)
              for source = (ignore-errors
                            (sb-introspect:find-definition-source callee))
              collect (or (and source
                               (definition-source-place
                                (definition-name-string name) source))
                          (definition-name-string name))))
      (loop for (name . source) in (ignore-errors (funcall relation symbol))
            collect (or (definition-source-place (definition-name-string name)
                                                 source)
                        (definition-name-string name)))))

(define-command (com-xref :command-table luvcraft-agent :name "Xref")
    ((relation 'string :prompt "callers, callees, references, binds, sets, macroexpands, or specializes")
     (name 'string :prompt "symbol"))
  "Cross-reference a Lisp name in the running game.  RELATION is callers (who calls this function), callees (what it calls), references/binds/sets (who uses this variable), macroexpands (who uses this macro), or specializes (methods specialized on this class).  Each answer gives the definition, its file:line, and its first source line."
  (let ((entry (assoc (string-trim " " relation) *xref-relations*
                      :test #'string-equal))
        (symbols (resolve-introspection-symbols name)))
    (cond
      ((null entry)
       (format nil "Unknown relation ~A; use one of ~{~A~^, ~}."
               relation (mapcar #'car *xref-relations*)))
      ((null symbols) (no-symbol-named name))
      (t
       (with-output-to-string (stream)
         (dolist (symbol symbols)
           (let* ((places (xref-places (cdr entry) symbol))
                  (shown (subseq places 0 (min (length places)
                                               *introspection-result-limit*))))
             (format stream "~A of ~A:~%" (string-capitalize (car entry))
                     (qualified-name symbol))
             (if (null places)
                 (format stream "  none recorded~%")
                 (dolist (place shown)
                   (if (stringp place)
                       (format stream "~A~%" place)
                       (progn
                         (write-source-place-header place stream)
                         (format stream "  ~A~%"
                                 (first-line (source-place-text place)))))))
             (when (> (length places) (length shown))
               (format stream "... and ~D more.~%"
                       (- (length places) (length shown))))
             (terpri stream))))))))

(define-command (com-source :command-table luvcraft-agent :name "Source")
    ((name 'string :prompt "symbol"))
  "Read the source text of a Lisp name's definitions in the running game, with file and line, exactly as written in the checkout.  Long definitions are cut after a screenful."
  (let ((symbols (resolve-introspection-symbols name)))
    (if (null symbols)
        (no-symbol-named name)
        (let ((places (mapcan #'symbol-source-places symbols)))
          (if (null places)
              (format nil "~A has no recorded source file." name)
              (with-output-to-string (stream)
                (dolist (place places)
                  (write-source-place-header place stream)
                  (format stream "~A~%~%"
                          (clipped-lines (source-place-text place)
                                         *introspection-source-lines*)))))))))
