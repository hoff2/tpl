;;;; klein-ast.lisp
;;;; abstract syntax tree classes for Klein
;;;; Chuck Hoffman
;;;; Translation of Programming Languages
;;;; Fall 2007

;;;; general base AST classes =================================================

(defclass abstract-syntax () ())

(defgeneric build (node semantic-stack current-token)
  (:documentation
   "classes for AST nodes must accept this message and return the new state of~
    the semantic-stack."))

(defun print-indent (indent)
  (format t "~&")
  (dotimes (x indent) (format t " ")))

(defgeneric print-ast (node &optional indent)
  (:documentation "for printing out contents of an AST node"))

(defmethod build ((node abstract-syntax) sem tok)
  (declare (ignore tok))
  (push node sem)
  sem)

(defclass ast-sequence (abstract-syntax)
  ((seq :initform nil
	:accessor seq)))

(defclass ast-sequenced (abstract-syntax)
  ())

(defmethod print-ast ((node ast-sequence) &optional (indent 0))
  (mapcar #'(lambda (n) (print-ast n (1+ indent))) (seq node)))

(defmethod add-to ((node ast-sequence) (new ast-sequenced))
  (setf (seq node) (append (seq node) (list new))))

(defmethod build ((node ast-sequenced) sem tok)
  (declare (ignore tok))
  (add-to (car sem) node)
  sem)

(defclass ast-expression (abstract-syntax)
  ())


;;;; identifiers, literals and other atoms ===================================

(defclass ast-identifier (ast-expression)
  ((name :accessor name)
   (line :accessor line)))

(defmethod build ((node ast-identifier) sem tok)
  (setf (name node) (token-text tok))
  (setf (line node) (token-line tok))
  (push node sem)
  sem)

(defmethod print-ast ((node ast-identifier) &optional (indent 0))
  (print-indent indent)
  (format t "identifier: ~A~%" (name node)))

(defclass ast-literal (ast-expression)
  ())

(defmethod print-ast ((node ast-literal) &optional (indent 0))
  (print-indent indent)
  (formal t "literal: ~S" (name node)))

(defclass ast-literalint (ast-literal)
  ((value :accessor value)
   (line  :accessor line)))

(defmethod build ((node ast-literalint) sem tok)
  (let ((val (parse-integer (token-text tok))))
    (if (>= val (expt 2 32))
	(error "Number too large: ~A line ~D" (token-text tok) (token-line tok))
      (progn
	(setf (value node) val)
	(setf (line node) (token-line tok))
	(push node sem)
	sem))))

(defmethod print-ast ((node ast-literalint) &optional (indent 0))
  (print-indent indent)
  (format t "literal int: ~D~%" (value node)))

(defclass ast-literalbool (ast-literal)
  ())

(defclass ast-literalboolt (ast-literalbool)
  ((name :reader name :initform 'true)))

;(defmethod name ((node ast-literalboolt))
;  'true)

(defclass ast-literalboolf (ast-literalbool)
  ((name :reader name :initform 'false)))

;(defmethod name ((node ast-literalboolf))
;  'false)

(defclass ast-type (abstract-syntax)
  ())

(defclass ast-typeint (ast-type)
  ((name :reader name :initform 'int)))

;(defmethod name ((node ast-typeint))
;  'int)

(defclass ast-typebool (ast-type)
  ((name :reader name :initform 'boolean)))

;(defmethod name ((node ast-typebool))
;  'boolean)


;;;; binary operations ========================================================

(defclass ast-binaryop (ast-expression)
  ((left  :accessor left)
   (right :accessor right)
   (line  :accessor line)))

(defmethod set-left-side ((node ast-binaryop) (expr ast-expression))
  (setf (left node) expr))

(defmethod set-right-side ((node ast-binaryop) (expr ast-expression))
  (setf (right node) expr))

(defmethod build ((node ast-binaryop) sem tok)
  (set-right-side node (pop sem))
  (set-left-side  node (pop sem))
  (setf (line node) (token-line tok))
  (push node sem)
   sem)

(defmethod print-ast ((node ast-binaryop) &optional (indent 0))
  (print-indent indent)
  (format t "left:~%")
  (print-ast (left node) (1+ indent))
  (print-indent indent)
  (format t "right:~%")
  (print-ast (right node) (1+ indent)))

(defclass ast-compareop (ast-binaryop)
  ())

(defclass ast-lessop (ast-compareop)
  ())

(defmethod print-ast ((node ast-lessop) &optional (indent 0))
  (print-indent indent)
  (format t "<")
  (call-next-method node (1+ indent)))

(defclass ast-equalsop (ast-compareop)
  ())

(defmethod print-ast ((node ast-equalsop) &optional (indent 0))
  (print-indent indent)
  (format t "=")
  (call-next-method node (1+ indent)))

(defclass ast-binarylogicalop (ast-binaryop)
  ())

(defclass ast-orop (ast-binarylogicalop)
  ())

(defmethod print-ast ((node ast-orop) &optional (indent 0))
  (print-indent indent)
  (format t "or")
  (call-next-method node (1+ indent)))

(defclass ast-binaryarithop (ast-binaryop)
  ())

(defclass ast-plusop (ast-binaryarithop)
  ())

(defmethod print-ast ((node ast-plusop) &optional (indent 0))
  (print-indent indent)
  (format t "+")
  (call-next-method node (1+ indent)))

(defclass ast-minusop (ast-binaryarithop)
  ())

(defmethod print-ast ((node ast-minusop) &optional (indent 0))
  (print-indent indent)
  (format t "-")
  (call-next-method node (1+ indent)))

(defclass ast-timesop (ast-binaryarithop)
  ())

(defmethod print-ast ((node ast-timesop) &optional (indent 0))
  (print-indent indent)
  (format t "*")
  (call-next-method node (1+ indent)))

(defclass ast-divideop (ast-binaryarithop)
  ())

(defmethod print-ast ((node ast-divideop) &optional (indent 0))
  (print-indent indent)
  (format t "/")
  (call-next-method node (1+ indent)))


;;;; unary operations =========================================================

(defclass ast-unaryop (ast-expression)
  ((operand :accessor operand)
   (line    :accessor line)))

(defmethod set-operand ((node ast-unaryop) (expr ast-expression))
  (setf (operand node) expr))

(defmethod build ((node ast-unaryop) sem tok)
  (set-operand node (pop sem))
  (setf (line node) (token-line tok))
  (push node sem)
   sem)

(defclass ast-printop (ast-unaryop)
  ())

(defmethod print-ast ((node ast-printop) &optional (indent 0))
  (print-indent indent)
  (format t "print")
  (print-ast (operand node) (1+ indent)))

(defclass ast-notop (ast-unaryop)
  ())

(defmethod print-ast ((node ast-notop) &optional (indent 0))
  (print-indent indent)
  (format t "not")
  (print-ast (operand node) (1+ indent)))


;;;; other compound contructs =================================================

;;;; program ==================================================================

(defclass ast-program (abstract-syntax)
  ((definitions  :accessor definition-seq)
   (symbol-table :accessor symbol-table)))

(defclass ast-definitionseq (ast-sequence)
  ())

(defmethod set-definitions ((node ast-program) (seq ast-definitionseq))
  (setf (definition-seq node) seq))

(defmethod build ((node ast-program) sem tok)
  (declare (ignore tok))
  (set-definitions node (pop sem))
  (push node sem)
  sem)

(defmethod print-ast ((node ast-program) &optional (indent 0))
  (print-ast (definition-seq node) indent))

(defmethod get-function-names ((node ast-program))
  (mapcar #'(lambda (def) (name (name-identifier def)))
	  (seq (definition-seq node))))

(defmethod get-main-function ((node ast-program))
  (find-if
   #'(lambda (def) (equalp "main" (get-name def)))
   (seq (definition-seq node))))


;;;; (function) definition ====================================================

(defclass ast-definition (ast-sequenced)
  ((name         :accessor name-identifier)
   (formals      :accessor parameter-seq)
   (type         :accessor return-type)
   (body         :accessor body)
   (symbol-table :accessor symbol-table)))

(defclass ast-formalseq (ast-sequence)
  ())

(defclass ast-formal (ast-sequenced)
  ((name :accessor param-identifier)
   (type :accessor param-type)))

(defmethod set-name-id ((def ast-definition) (id ast-identifier))
  (setf (name-identifier def) id))

(defmethod get-name ((def ast-definition))
  (name (name-identifier def)))

(defmethod set-parameters ((def ast-definition) (formals ast-formalseq))
  (setf (parameter-seq def) formals))

(defmethod set-return-type ((def ast-definition) (type ast-type))
  (setf (return-type def) type))

(defmethod set-body ((def ast-definition) (expr ast-expression))
  (setf (body def) expr))

(defmethod add-to ((seq ast-definitionseq) (new ast-definition))
  (call-next-method))

(defmethod build ((node ast-definition) sem tok)
  (set-body        node (pop sem))
  (set-return-type node (pop sem))
  (set-parameters  node (pop sem))
  (set-name-id     node (pop sem))
  (call-next-method node sem tok))

(defmethod add-to ((seq ast-formalseq) (new ast-formal))
  (call-next-method))

(defmethod set-name-id ((formal ast-formal) (id ast-identifier))
  (setf (param-identifier formal) id))

(defmethod set-type ((formal ast-formal) (type ast-type))
  (setf (param-type formal) type))

(defmethod get-name ((formal ast-formal))
  (name (param-identifier formal)))

(defmethod build ((node ast-formal) sem tok)
  (set-type    node (pop sem))
  (set-name-id node (pop sem))
  (call-next-method node sem tok))

(defmethod print-ast ((def ast-definition) &optional (indent 0))
  (print-indent indent)
  (format t "definition of ~A -- returns ~S.  params:~%"
	  (get-name def) (name (return-type def)))
  (print-ast (parameter-seq def) (1+ indent))
  (print-indent indent)
  (format t "body:~%")
  (print-ast (body def) (1+ indent)))

(defmethod print-ast ((formal ast-formal) &optional (indent 0))
  (print-indent indent)
  (format t "~A (~S)~%" (get-name formal) (name (param-type formal))))

(defmethod get-arity ((node ast-definition))
  (length (seq (parameter-seq node))))


;;;; function call ============================================================

(defclass ast-call (ast-expression)
  ((identifier :accessor function-identifier)
   (actuals    :accessor arg-seq)
   (line       :accessor line)))

(defclass ast-actualseq (ast-sequence)
  ())

(defclass ast-actual (ast-expression ast-sequenced)
  ((expr :accessor expr)))

(defmethod set-function-id ((call ast-call) (id ast-identifier))
  (setf (function-identifier call) id))

(defmethod set-arg-seq ((call ast-call) (args ast-actualseq))
  (setf (arg-seq call) args))

(defmethod get-function-name ((call ast-call))
  (name (function-identifier call)))

(defmethod get-arity ((node ast-call))
  (length (seq (arg-seq node))))

(defmethod build ((node ast-call) sem tok)
  (set-arg-seq node (pop sem))
  (set-function-id node (pop sem))
  (setf (line node) (token-line tok))
  (push node sem)
  sem)

(defmethod add-to ((seq ast-actualseq) (new ast-actual))
  (call-next-method))

(defmethod set-content ((actual ast-actual) (expr ast-expression))
  (setf (expr actual) expr))

(defmethod build ((node ast-actual) sem tok)
  (set-content node (pop sem))
  (call-next-method node sem tok))

(defmethod print-ast ((call ast-call) &optional (indent 0))
  (print-indent indent)
  (format t "call to ~A with args:~%" (get-function-name call))
  (print-ast (arg-seq call) (1+ indent)))

(defmethod print-ast ((actual ast-actual) &optional (indent 0))
  (print-ast (expr actual) indent))


;;;; if expression ============================================================

(defclass ast-ifexpr (ast-expression)
  ((test-expr :accessor test-expr)
   (if-part   :accessor if-part)
   (else-part :accessor else-part)
   (line      :accessor line)))

(defmethod set-test ((ifexpr ast-ifexpr) (expr ast-expression))
  (setf (test-expr ifexpr) expr))

(defmethod set-if-part ((ifexpr ast-ifexpr) (expr ast-expression))
  (setf (if-part ifexpr) expr))

(defmethod set-else-part ((ifexpr ast-ifexpr) (expr ast-expression))
  (setf (else-part ifexpr) expr))

(defmethod build ((node ast-ifexpr) sem tok)
  (set-else-part node (pop sem))
  (set-if-part   node (pop sem))
  (set-test      node (pop sem))
  (setf (line node) (token-line tok))
  (push node sem)
  sem)

(defmethod print-ast ((ifexpr ast-ifexpr) &optional (indent 0))
  (print-indent indent)
  (format t "if-expression~%")
  (print-indent indent)
  (format t "test:~%")
  (print-ast (test-expr ifexpr) (1+ indent))
  (print-indent indent)
  (format t "if-part:~%")
  (print-ast (if-part ifexpr) (1+ indent))
  (print-indent indent)
  (format t "else-part:~%")
  (print-ast (else-part ifexpr) (1+ indent)))


;; convenience accessors ======================================================

(defmethod definitions-list ((program ast-program))
  (seq (definition-seq program)))

(defmethod name-string ((definition ast-definition))
  (name (name-identifier definition)))

(defmethod name-string ((formal ast-formal))
  (name (param-identifier formal)))

(defmethod parameters-list ((definition ast-definition))
  (seq (parameter-seq definition)))

