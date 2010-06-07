(defparameter *ac-grammar*
  '((program         (ndeclarations 
		      declarations
		      nstatements
		      statements
		      nprogram))
    (declarations    (declaration declarations))
    (declarations    (empty))
    (declaration     (floatdecl identifier nidentifier nfloatdecl))
    (declaration     (intdecl identifier nidentifier nintdecl))
    (statements      (statement statements))
    (statements      (empty))
    (statement       (identifier
		      nidentifier
		      assign
		      value
		      expression-tail
		      nassignment))
    (statement       (print identifier nidentifier nprintstmt))
    (expression-tail (plus value nplusop expression-tail))
    (expression-tail (minus value nminusop expression-tail))
    (expression-tail (empty))
    (value           (identifier nidentifier))
    (value           (float-lit nlitfloat))
    (value           (int-lit nlitint))))

(defun fftests-ac ()
  (setup-parser :grammar      *ac-grammar*
		:ast-types    *ac-asts*
		:consume-func nil
		:peek-func    nil)
  (assert (set= (first-set 'value) '(identifier float-lit int-lit)))
  (assert (set= (first-set 'expression-tail) '(plus minus empty)))
  (assert (set= (first-set 'statement) '(identifier print)))
  (assert (set= (first-set 'statements) '(identifier print empty)))
  (assert (set= (first-set 'declaration) '(floatdecl intdecl)))
  (assert (set= (first-set 'declarations) '(floatdecl intdecl empty)))
  (assert (set= (first-set 'program)
		'(floatdecl intdecl identifier print empty)))
  (assert (set= (follow-set 'program) '(eof)))
  (assert (set= (follow-set 'declarations) '(identifier print eof)))
  (assert (set= (follow-set 'declaration)
		'(floatdecl intdecl identifier print eof)))
  (assert (set= (follow-set 'statements) '(eof)))
  (assert (set= (follow-set 'statement) '(identifier print eof)))
  (assert (set= (follow-set 'expression-tail) '(identifier print eof)))
  (assert (set= (follow-set 'value) '(plus minus identifier print eof))))


(defmethod build ((node abstract-syntax) sem tok)
  (format t "build~%  token: ~s~%  semantic: ~s~%" tok sem))


(defgeneric print-ast (node &optional indent))
(defgeneric add-to (node child))

(defclass nprogram (abstract-syntax)
  ((declarations :accessor declarations)
   (statements   :accessor statements)))
(defmethod build ((node nprogram) sem tok)
  (setf (statements node) (pop sem))
  (setf (declarations node) (pop sem))
  (cons node sem))
(defmethod print-ast ((node nprogram) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<program>~%")
  (print-ast (declarations node) (+ indent 1))
  (print-ast (statements node) (+ indent 1)))

(defclass ndeclarations (abstract-syntax)
  ((declarations :accessor declarations :initform nil)))
(defmethod build ((node ndeclarations) sem tok)
  (cons node sem))
(defmethod add-to ((node ndeclarations) (child ndeclaration))
  (setf (declarations node) (append (declarations node) (list child))))
(defmethod print-ast ((node ndeclarations) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<declarations>~%")
  (mapcar #'(lambda (decl) (print-ast decl (+ indent 1))) (declarations node)))

(defclass ndeclaration (abstract-syntax)
  ((identifier :accessor identifier)))
(defmethod build ((node ndeclaration) sem tok)
  (setf (identifier node) (pop sem))
  (add-to (car sem) node)
  sem)
; node is added as child of ndeclarations node already on stack
; return stack as is after popping identifier node

(defclass nfloatdecl (ndeclaration) ())
(defmethod print-ast ((node nfloatdecl) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<float declaration ~A>~%" (value (identifier node))))
(defclass nintdecl (ndeclaration) ())
(defmethod print-ast ((node nintdecl) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<int declaration ~A>~%" (value (identifier node))))

(defclass nstatements (abstract-syntax)
  ((statements :accessor statements :initform nil)))
(defmethod build ((node nstatements) sem tok)
  (cons node sem))
(defmethod add-to ((node nstatements) (child nstatement))
  (setf (statements node) (append (statements node) (list child))))
(defmethod print-ast ((node nstatements) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<statements>~%")
  (mapcar #'(lambda (stmt) (print-ast stmt (+ indent 1))) (statements node)))

(defclass nstatement (abstract-syntax) ())
;(defmethod build ((node nstatement) sem tok)
;  (add-to (car sem) node)
;  sem)
(defclass nassignment (nstatement)
  ((identifier :accessor identifier)
   (value      :accessor value)))
(defmethod build ((node nassignment) sem tok)
  (setf (value node) (pop sem))
  (setf (identifier node) (pop sem))
  (add-to (car sem) node)
  sem)
;  (cons node sem))
(defmethod print-ast ((node nassignment) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<assignment to ~A>~%" (value (identifier node)))
  (print-ast (value node) (+ indent 1)))
  
(defclass nprintstmt (nstatement)
  ((identifier :accessor identifier)))
(defmethod build ((node nprintstmt) sem tok)
  (setf (identifier node) (pop sem))
  (add-to (car sem) node)
  sem)
;  (cons node sem))
(defmethod print-ast ((node nprintstmt) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<print statement of ~A>~%" (value (identifier node))))

(defclass nbinaryop (abstract-syntax)
  ((left  :accessor left)
   (right :accessor right)))
(defclass nplusop (nbinaryop) ())
(defmethod print-ast ((node nplusop) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<addition operation>~%")
  (print-ast (left node) (+ indent 1))
  (print-ast (right node) (+ indent 1)))
(defclass nminusop (nbinaryop) ())
(defmethod print-ast ((node nminusop) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "<subtraction operation>~%")
  (print-ast (left node) (+ indent 1))
  (print-ast (right node) (+ indent 1)))
(defmethod build ((node nbinaryop) sem tok)
  (setf (right node) (pop sem))
  (setf (left node) (pop sem))
  (cons node sem))

(defclass nvalue (abstract-syntax)
  ((value :accessor value)))
(defclass nidentifier (nvalue) ())
(defclass nlitfloat (nvalue) ())
(defclass nlitint (nvalue) ())
(defmethod build ((node nvalue) sem tok)
  (setf (value node) (token-text tok))
  (cons node sem))
(defmethod print-ast ((node nvalue) &optional (indent 0))
  (dotimes (x indent) (format t " "))
  (format t "~A~%" (value node)))

(defparameter *ac-asts*
  '(nprogram
    ndeclarations ndeclaration nfloatdecl nintdecl
    nstatements
    nstatement nassignment nprintstmt
    nbinaryop nplusop nminusop
    nvalue nidentifier nlitfloat nlitint))

(defparameter *sample-program*
  '((intdecl "i" 1) (identifier "a" 1)
    (floatdecl "f" 2) (identifier "b" 2)
    (identifier "a" 3) (assign "=" 3) (int-lit "5" 3)
    (identifier "b" 4) (assign "=" 4) (identifier "a" 4) (plus "+" 4)
    (float-lit "3.2" 4)
    (print "p" 5) (identifier "b" 5)
    (eof "" 5)))

(defun consume-and-peek (program)
  (list
   #'(lambda () (pop program))
   #'(lambda () (car program))))

(defun setup-ac (program)
  (let ((functions (consume-and-peek program)))
    (setup-parser :grammar   *ac-grammar*
		  :ast-types *ac-asts*
		  :consume-func (first functions)
		  :peek-func (second functions)
		  :print t)))

(defun attempt-ac (program)
  (setup-ac program)
  (parse))
