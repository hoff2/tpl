;;;; klein-semantic.lisp
;;;; semantic analyzer for klein
;;;; Chuck Hoffman
;;;; Translation of Programming Languages, Fall 2007

;;; ===========================================================================
;;; "type error" type.  A new type error will cause an error message
;;; immediately but will not halt analysis.

(defun new-type-error (message)
  (progn
    (format t "~A~%" message)
    (list 'err message)))

(defun is-type-error (type)
  (and (listp type)
       (eq (car type) 'err)))

;; An type error in a subconstruct is propagated upwards with this function
;; so as not to effect a cascade of type error messages
(defun propagate-type-error ()
  (list 'err "propagated"))


;;; ==========================================================================
;;; symbol table

;; symbol table is represented as a hash table
(defgeneric make-symbol-table (construct &optional enclosing-table))
(defgeneric st-insert (id type lexindex table))
(defgeneric st-lookup-type (id table))

;; another other symbol table for the enclosing scope is linked to it
(defun get-enclosing-scope (symbol-table)
  (gethash '*enclosing* symbol-table))

(defun set-enclosing-scope (local enclosing)
  (setf (gethash '*enclosing* local) enclosing))

;; a symbol table entry is a list consisting of a type expression, the line
;; number at which the symbol is declared, a list of line numbers where
;; it is used, and a "lexical index" number reflecting the order in which
;; declarations appear.
(defun make-st-entry (type decl-line ref-lines lexindex)
  (list type decl-line ref-lines lexindex))

(defun st-entry-type (st-entry)
  (first st-entry))

(defun st-entry-declared-line (st-entry)
  (second st-entry))

(defun st-entry-references (st-entry)
  (third st-entry))

(defun st-entry-lexindex (st-entry)
  (fourth st-entry))

(defun add-reference (st-entry line)
  (make-st-entry (st-entry-type st-entry)
		 (st-entry-declared-line st-entry)
		 (cons line (st-entry-references st-entry))
		 (st-entry-lexindex st-entry)))

(defun print-st-entry (st-entry)
  (format t
	  (concatenate 'string
		       "type: ~S~%"
		       "lexindex: ~D~%"
		       "declared at line ~D~%"
		       "referenced at lines ~S~%~%")
	  (st-entry-type          st-entry)
	  (st-entry-lexindex      st-entry)
	  (st-entry-declared-line st-entry)
	  (st-entry-references    st-entry)))

;; insert is done by inserting into the referenced table -- unless it turns out
;; that the given symbol is already present there, which is an error
(defun add-entry (symbol table new-entry)
  (setf (gethash symbol table) new-entry))

(defmethod st-insert ((id ast-identifier) type lexindex table)
  (let ((symbol (name id)))
    (if (not (null (gethash symbol table)))
	(error "Duplicate declaration: ~A~%" symbol)
      (add-entry symbol table (make-st-entry type (line id) nil lexindex)))))

;; an entry can be modified (this is mainly used to add new elements to the
;; list of line numbers where the symbol is referenced).  modifying the entry
;; involves looking it up by the same method used for lookup (below).
(defun change-entry (symbol table new-entry)
  (if (null (gethash symbol table))
      (let ((enclosing-scope (get-enclosing-scope table)))
	(if (null enclosing-scope)
	    (error "Cannot change-entry a nonexistant symbol table entry")
	  (change-entry symbol (get-enclosing-scope table) new-entry)))
    (setf (gethash symbol table) new-entry)))

;; lookup is done by first checking the top-level table, and then, if not
;; found there, recursively checking any enclosing-scope tables; if symbol
;; is never found, returns null.  st-lookup-type accepts either ast-identifier
;; objects or the plain string-name of a symbol.
(defun st-entry (symbol table)
  (let ((found (gethash symbol table))
	(enclosing-scope (get-enclosing-scope table)))
    (if (null found)
	(if (null enclosing-scope)
	    nil
	  (st-entry symbol enclosing-scope))
      found)))

(defmethod st-lookup-type (plain-symbol table)
  (let ((entry (st-entry plain-symbol table)))
    (if entry (st-entry-type entry))))

(defmethod st-lookup-type ((id ast-identifier) table)
  (let* ((symbol (name id))
	 (entry (st-entry symbol table)))
    (change-entry symbol table (add-reference entry (line id)))
    (st-entry-type entry)))

;; lexical address is 2-item list: (number-of-scope-levels-up lexindex)
(defmethod st-lookup-lexaddr (plain-symbol table &optional (level 0))
  (let ((found (gethash plain-symbol table))
	(enclosing-scope (get-enclosing-scope table)))
    (if (null found)
	(if (null enclosing-scope)
	    nil
	  (st-lookup-lexaddr plain-symbol enclosing-scope (1+ level)))
      (list level (st-entry-lexindex found)))))

(defmethod st-lookup-lexaddr ((id ast-identifier) table &optional (level 0))
  (st-lookup-lexaddr (name id) table level))

;; Make symbol table for entire program (according to klein
;; specification, this will contain all function names -> types 
(defmethod make-symbol-table ((program ast-program) &optional blah)
  (declare (ignore blah))
  (let ((table (make-hash-table :test #'equal))
	(deflist (definitions-list program)))
    (dotimes (lexindex (length deflist))
      (st-insert (name-identifier (elt deflist lexindex))
		 (get-static-type (elt deflist lexindex))
		 lexindex
		 table))
    (setf (symbol-table program) table)
    table))

;;; make symbol table for function definition (according to klein
;;; specification, this will contain parameter names & types)
(defmethod make-symbol-table
  ((function ast-definition) &optional (enclosing nil))
  (let ((table (make-hash-table :test #'equal))
	(paramlist (parameters-list function)))
    (set-enclosing-scope table enclosing)
    (dotimes (lexindex (length paramlist))
      (st-insert (param-identifier (elt paramlist lexindex))
		 (get-static-type  (elt paramlist lexindex))
		 lexindex
		 table))
    (setf (symbol-table function) table)
    table))


;;; ===========================================================================
;;; get-static-type is used to get types for manifestly typed constructs
;;; (function definitions and their parameters, literals, and type names
;;; themselves) wherein no symbol table lookup is needed.

(defgeneric get-static-type (construct))

(defmethod get-static-type ((literal ast-literalint)) 'int)

(defmethod get-static-type ((literal ast-literalbool)) 'boolean)

(defmethod get-static-type ((type ast-type)) (name type))

(defmethod get-static-type ((sequence ast-sequence))
  (mapcar #'get-static-type (seq sequence)))

(defmethod get-static-type ((formal ast-formal))
  (get-static-type (param-type formal)))

;; A function definition's type is given as a list of: the symbol 'function,
;; a list of types of parameters, return type.
(defmethod get-static-type ((definition ast-definition))
  (list 'function
	(get-static-type (parameter-seq definition))
	(get-static-type (return-type definition))))

(defun is-function (functype)
  (and (listp functype)
       (eq 'function (first functype))))

(defun func-param-types (functype) (second functype))

(defun func-return-type (functype) (third functype))

;;; ===========================================================================
;;; get-type is used to infer types of expressions where a symbol table is
;;; needed.  To aid in structural recursion, get-type can delegate to
;;; get-static-type when possible.

(defgeneric get-type (construct symbol-table))

(defmethod get-type ((sequence ast-sequence) symbol-table)
  (let ((types
	 (mapcar #'(lambda (element) (get-type element symbol-table))
		 (seq sequence))))
    (if (find #'is-type-error types) (propagate-type-error) types)))

(defmethod get-type ((identifier ast-identifier) symbol-table)
  (or (st-lookup-type identifier symbol-table)
      (new-type-error
       (concatenate 'string
		    "Undeclared identifier "
		    (name identifier)
		    " found at line "
		    (write-to-string (line identifier))))))

(defmethod get-type ((literal ast-literal) symbol-table)
  (declare (ignore symbol-table))
  (get-static-type literal))

;; = and < compare integers only
(defmethod get-type ((operation ast-compareop) symbol-table)
  (let ((left-type  (get-type (left  operation) symbol-table))
	(right-type (get-type (right operation) symbol-table)))
    (cond ((or (is-type-error left-type)
	      (is-type-error right-type)) (propagate-type-error))
	  ((and (eq left-type  'int)
		(eq right-type 'int)) 'boolean)
	  (t (new-type-error
	      (concatenate 'string
			   "Improper operand types for integer comparison "
			   "at line "
			   (write-to-string (line operation))))))))

;; "or" operator
(defmethod get-type ((operation ast-binarylogicalop) symbol-table)
  (let ((left-type  (get-type (left  operation) symbol-table))
	(right-type (get-type (right operation) symbol-table)))
    (cond ((or (is-type-error left-type)
	       (is-type-error right-type)) (propagate-type-error))
	  ((and (eq left-type  'boolean)
		(eq right-type 'boolean)) 'boolean)
	  (t (new-type-error
	      (concatenate 'string
			   "Improper operand types for boolean connective "
			   "at line "
			   (write-to-string (line operation))))))))

;; + - * /
(defmethod get-type ((operation ast-binaryarithop) symbol-table)
  (let ((left-type  (get-type (left  operation) symbol-table))
	(right-type (get-type (right operation) symbol-table)))
    (cond ((or (is-type-error left-type)
	       (is-type-error right-type)) (propagate-type-error))
	  ((and (eq left-type  'int)
		(eq right-type 'int)) 'int)
	  (t (new-type-error
	      (concatenate 'string
			   "Improper operand types for arithmetic expression "
			   "at line "
			   (write-to-string (line operation))))))))

;; print
(defmethod get-type ((operation ast-printop) symbol-table)
  (get-type (operand operation) symbol-table))

;; not
(defmethod get-type ((operation ast-notop) symbol-table)
  (let ((operand-type (get-type (operand operation) symbol-table)))
    (cond ((is-type-error operand-type) (propagate-type-error))
	  ((eq operand-type 'boolean) 'boolean)
	  (t (new-type-error
	      (concatenate 'string
			   "Improper operand type for \"not\" at line "
			   (write-to-string (line operation))))))))

;; function calls:
;; compare type-list of call and definition (equal length and contents)
(defmethod get-type ((callexpr ast-call) symbol-table)
  (let* ((called-id   (function-identifier callexpr))
	 (called-type (get-type called-id symbol-table))
	 (arg-types   (get-type (arg-seq callexpr) symbol-table)))
    (cond ((is-type-error arg-types) (propagate-type-error))
	  ((not (is-function called-type))
	   (new-type-error
	    (concatenate 'string
			 "Not the name of a function: "
			 (name called-id)
			 " at line "
			 (write-to-string (line callexpr)))))
	  ((equalp arg-types (func-param-types called-type))
	   (func-return-type called-type))
	  (t (new-type-error
	      (concatenate 'string
			   "Improper arguments to "
			   (name called-id)
			   " at line "
			   (write-to-string (line callexpr))))))))

;; arguments to function calls:
(defmethod get-type ((arg ast-actual) symbol-table)
  (let ((type (get-type (expr arg) symbol-table)))
    (if (is-type-error type) (propagate-type-error) type)))

;; if expressions
(defmethod get-type ((ifexpr ast-ifexpr) symbol-table)
  (let ((test-type (get-type (test-expr ifexpr) symbol-table))
	(if-type   (get-type (if-part   ifexpr) symbol-table))
	(else-type (get-type (else-part ifexpr) symbol-table)))
    (cond
     ((or (is-type-error test-type)
	  (is-type-error if-type)
	  (is-type-error else-type)) (propagate-type-error))
     ((not (eq test-type 'boolean))
      (new-type-error
       (concatenate 'string
		    "Improper predicate expression for if expression "
		    "at line "
		    (write-to-string (line ifexpr)))))
     ((not (eq if-type else-type))
      (new-type-error
       (concatenate 'string
		    "Branches of if-expression must be of same type "
		    "at line "
		    (write-to-string (line ifexpr)))))
     (t if-type))))


;;;; ==========================================================================
;;; main functions.

;; type-checks the ast of a klein program, including its function definitions,
;; and stores the symbol table in the ast
(defmethod type-check-program ((program ast-program))
  (let ((main-symbol-table (make-symbol-table program)))
    (if (not (is-function (st-lookup-type "main" main-symbol-table)))
	(error "No main() function found")
      (find-if #'is-type-error
	       (mapcar
		#'(lambda (definition)
		    (type-check-function definition main-symbol-table))
		(definitions-list program))))))

;; type-check a function definition within a klein program and stores its
;; symbol table in the ast.  requires the symbol table for the enclosing scope
;; (main program) as a parameter so that function calls can by type-checked.
;; type-check-program uses this method to type-check functions.
(defmethod type-check-function ((function ast-definition) enclosing)
  (let* ((local-symbol-table (make-symbol-table function enclosing))
	 (body-type (get-type (body function) local-symbol-table))
	 (return-type (func-return-type (get-static-type function))))
    (cond ((is-type-error body-type) (propagate-type-error))
	  ((not (eq body-type return-type))
	   (new-type-error
	    (concatenate 'string
			 "Body of function "
			 (name-string function)
			 " does not match declared return type"))))))

;; prints the symbol table of a klein program and its function definitions.
;; requires that the symbol tables have already been built and stored in the
;; ast (so execute type-check-program first)
(defmethod print-symbol-table ((program ast-program))
  (format t "=== MAIN PROGRAM ===~%")
  (maphash #'(lambda (name entry)
	       (progn
		 (format t "name: ~A~%" name)
		 (print-st-entry entry)))
	   (symbol-table program))
  (mapcar #'print-symbol-table (definitions-list program)))

;; prints the symbol table of a function definition in the ast
(defmethod print-symbol-table ((function ast-definition))
  (format t "=== function ~A ===~%" (name-string function))
  (maphash #'(lambda (name entry)
	       (if (not (eq name '*enclosing*))
		   (progn
		     (format t "name: ~A~%" name)
		     (print-st-entry entry))))
	   (symbol-table function)))
