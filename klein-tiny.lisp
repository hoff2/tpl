;;; klein-tiny.lisp
;;; TM (Tiny Machine) code generator for Klein
;;; Chuck Hoffman
;;; Translation of Programming Languages
;;; Fall 2007

;; give some nice convenient names to some registers & offset values
(defparameter left       0)  ; register 0 for left operand, or single things
(defparameter right      1)  ; register 1 for right operand
(defparameter result     2)  ; register 2 for result
(defparameter misc1      4)  ; registers 3 and 4 for misc
(defparameter misc2      3)  ; (address manipulations mostly)
(defparameter sp         5)  ; register 5 for stack pointer
(defparameter fp         6)  ; register 6 for frame pointer
(defparameter pc         7)  ; register 7 is program counter, as we know

(defparameter returnval  0)  ; fp + returnval is where return value goes
(defparameter returnaddr 1)  ; fp + returnaddr is return-to address
(defparameter savefp     2)  ; fp + savefp is old fp
(defparameter args       3)  ; args start at fp + args

(defparameter top        0)  ; sp + top is top of temp stack
(defparameter under     -1)  ; sp + under is 2nd item on temp stack

;; generate a tiny instruction
(defun tiny-instr (opcode r s u &optional (comment ""))
  (list opcode r s u comment))

;; tiny instructions to push a temp to stack
(defun push-temp (register)
  (list (tiny-instr 'lda sp       1 sp "push-temp: increment sp")
	(tiny-instr 'st  register 0 sp "store at sp")))

;; tiny instructions to pop a temp from stack into a register
(defun pop-temp (register)
  (list (tiny-instr 'ld  register  top sp "pop-temp: load")
	(tiny-instr 'lda sp       -1   sp "decrement sp")))

;; tiny instructions to pop two temps from stack at once, with the top
;; temp to be considered the right operand and the one under it the left
(defun pop-temp-lr ()
  (list (tiny-instr 'ld  right top   sp "pop-temp-lr: load right")
	(tiny-instr 'ld  left  under sp "pop-temp-lr: load left")
	(tiny-instr 'lda sp    -2    sp "decrement sp by 2")))

(defmethod generate ((node ast-literalint))
  (cons (tiny-instr 'ldc result (value node) 0 "int literal")
	(push-temp result)))

(defmethod generate ((node ast-literalboolt))
  (declare (ignore node))
  (cons (tiny-instr 'ldc result 1 0 "literal true")
	(push-temp result)))

(defmethod generate ((node ast-literalboolf))
  (declare (ignore node))
  (cons (tiny-instr 'ldc result 0 0 "literal false")
	(push-temp result)))

(defmethod operate ((node ast-plusop))
  (list (tiny-instr 'add result left right "+")))

(defmethod operate ((node ast-minusop))
  (list (tiny-instr 'sub result left right "-")))

(defmethod operate ((node ast-timesop))
  (list (tiny-instr 'mul result left right "*")))

(defmethod operate ((node ast-divideop))
  (list (tiny-instr 'div result left right "/")))

(defmethod operate ((node ast-lessop))
  (list (tiny-instr 'sub left   left right "<: subtract right from left")
	(tiny-instr 'jlt left   2    pc    "result < 0 if left < right")
	(tiny-instr 'ldc result 0    0     "false")
	(tiny-instr 'lda pc     1    pc    "jump over")
	(tiny-instr 'ldc result 1    0     "true")))

(defmethod operate ((node ast-equalsop))
  (list (tiny-instr 'sub left   left right "=: subtract right from left")
	(tiny-instr 'jeq left   2    pc    "result = 0 if left = right")
	(tiny-instr 'ldc result 0    0     "false")
	(tiny-instr 'lda pc     1    pc    "jump over")
	(tiny-instr 'ldc result 1    0     "false")))

(defmethod generate ((node ast-orop))
  (let* ((right-part
	  (append
	   (generate (right node))
	   (pop-temp right)
	   (list
	    (tiny-instr 'jeq right  2 pc "if right = 0 skip")
	    (tiny-instr 'ldc result 1 0  "result = true")
	    (tiny-instr 'lda pc     1 pc "skip")
	    (tiny-instr 'ldc result 0 0  "result = false"))))
	 (left-part
	  (append 
	   (generate (left  node))
	   (pop-temp left)
	   (list
	    (tiny-instr 'jeq left   2 pc "or: if left = 0 jump to r")
	    (tiny-instr 'ldc result 1 0  "result = true")
	    (tiny-instr 'lda pc (length right-part) pc "short-circuit")))))
    (append left-part right-part (push-temp result))))

(defmethod operate ((node ast-notop))
  (list (tiny-instr 'jne left   2 pc "not: jump if not 0")
	(tiny-instr 'ldc result 1 0  "result = true")
	(tiny-instr 'lda pc     1 pc "jump over")
	(tiny-instr 'ldc result 0 0  "result = false")))

(defmethod generate ((node ast-binaryop))
  (append (generate (left  node))
	  (generate (right node))
	  (pop-temp-lr)
	  (operate node)
	  (push-temp result)))

(defmethod generate ((node ast-unaryop))
  (append (generate (operand node))
	  (pop-temp left)
	  (operate node)
	  (push-temp result)))

(defmethod generate ((node ast-ifexpr))
  (let ((truepart  (generate (if-part  node)))
	(falsepart (generate (else-part node))))
    (append (generate (test-expr node))
	    (pop-temp left)
	    (cons
	     (tiny-instr 'jeq left (1+ (length truepart)) pc "skip true case")
	     truepart)
	    (cons
	     (tiny-instr 'lda pc   (length falsepart)     pc "skip false case")
	     falsepart))))

;; an argument is passed by evaluating the expression and just leaving it
;; on the stack
(defmethod generate ((node ast-actual))
  (generate (expr node)))

(defun append-code-chunks (list-of-chunks)
  (if (null list-of-chunks) nil
    (append (car list-of-chunks)
	    (append-code-chunks (cdr list-of-chunks)))))

(defmethod generate ((node ast-actualseq))
  (append-code-chunks (mapcar #'generate (seq node))))

;; the following two leave untranslated identifiers in the generated code
;; (as their string names).  These must be looked up in a symbol table and
;; changed to their index numbers after generation:

(defmethod generate ((node ast-call))
  (let ((func-name (name (function-identifier node))))
    (append
     ; stack frame (fp) for new call will start at current sp + 1
     (list (tiny-instr 'ldc result 0          0      "setup new stack frame")
	   (tiny-instr 'st  result (1+ returnval) sp "return value")
	   (tiny-instr 'st  fp     (1+ savefp)    sp "save current fp")
	   (tiny-instr 'lda sp     args           sp "set sp to push args"))
     (generate (arg-seq node))
     (list (tiny-instr 'lda fp (- (+ 2 (get-arity node))) sp "compute new fp") 
	   (tiny-instr 'lda misc1  2              pc "compute return address")
	   (tiny-instr 'st  misc1  returnaddr     fp "store it")
	   (tiny-instr 'ldc pc     func-name      0  func-name)))))

;; for variable identifiers only -- generate ast-call does not use this
(defmethod generate ((node ast-identifier))
  (cons (tiny-instr 'ld result (name node) fp (name node))
	(push-temp result)))

;; looks for identifier-strings in second-operand position (presuming they name
;; local variables) and replaces them with their offset from frame pointer
;; (lexical index plus start-of-args)
(defun patch-variables (instruction symbol-table)
  (tiny-instr (first instruction)
	      (second instruction)
	      (if (stringp (third instruction))
		  (let ((lexaddr
			 (st-lookup-lexaddr (third instruction) symbol-table)))
		    (if (and lexaddr (= 0 (first lexaddr)))
			(+ args (second lexaddr))
		      (third instruction)))
		(third instruction))
	      (fourth instruction)
	      (fifth instruction)))

;; looks for identifier-strings in the second-operand position (presuming they
;; are function names being loaded as constants into pc) and replaces them
;; with their addresses (as computed earlier and given in the args here) 
(defun patch-function-names (instruction functable)
  (tiny-instr (first instruction)
	      (second instruction)
	      (if (stringp (third instruction))
		  ; jump 1 short because of pc increment
		  (1- (gethash (third instruction) functable))
		(third instruction))
	      (fourth instruction)
	      (fifth instruction)))
  
;; The result value of a call ends up just being left on top of the temp stack
;; of the caller.
(defmethod generate ((node ast-definition))
  (append
   (mapcar
    #'(lambda (instr) (patch-variables instr (symbol-table node)))
    (generate (body node)))
   (pop-temp result)                             ; get value of body from stack
   (list (tiny-instr 'st  result returnval  fp    "store return-value")
	 (tiny-instr 'lda sp     returnval  fp    "point sp to it")
	 (tiny-instr 'ld  misc1  returnaddr fp    "fetch return address")
	 (tiny-instr 'ld  fp     savefp     fp    "restore caller's fp")
	 (tiny-instr 'lda pc     0          misc1 "jump to return addr"))))

(defun function-start-addrs (func-list first-addr)
  (labels
      ((starts
	(lengths start-addr)
	(if (null lengths) nil  ;(list start)
	  (cons start-addr
		(starts (cdr lengths) (+ (car lengths) start-addr))))))
    (starts (mapcar #'length func-list) first-addr)))

(defun generate-mainargs-push (reg arity &optional (argnum 0))
  (if (= argnum arity) nil
    (append
     (list (tiny-instr 'ld  result argnum reg "command-line arg")
	   (tiny-instr 'st  result (+ argnum args) fp))
     (generate-mainargs-push reg arity (1+ argnum)))))

(defmethod generate ((node ast-program))
  (let* ((func-names (get-function-names node))
	 (functions (mapcar #'generate (seq (definition-seq node))))
	 (main-arity (get-arity (get-main-function node)))
	 (prelude
	  (append
	   (list (tiny-instr 'ldc  fp (1+ main-arity)  0  "first avail. addr")
		 (tiny-instr 'ldc  misc1  0            0)
		 (tiny-instr 'st   misc1  returnval    fp "first stack frame")
		 (tiny-instr 'st   fp     savefp       fp)
		 (tiny-instr 'lda  sp     (1- args)    fp "start stack")
		 (tiny-instr 'ldc  misc2  1            0  "addr of cmd-args"))
	   (generate-mainargs-push misc2 main-arity)
	   (list (tiny-instr 'lda sp main-arity sp "update sp") 
       (tiny-instr 'lda  misc1  2            pc "compute return addr")
		 (tiny-instr 'st   misc1  returnaddr   fp "store it")
		 (tiny-instr 'ldc  pc     "main"       0  "jump to main")
		 (tiny-instr 'ld   result returnval    fp "get return value")
		 (tiny-instr 'out  result 0            0  "print it")
		 (tiny-instr 'halt 0      0            0))))
	 (func-addrs (function-start-addrs functions (1+ (length prelude))))
	 (functable  (make-hash-table :test #'equal)))
    (mapcar
     #'(lambda (name addr) (setf (gethash name functable) addr))
     func-names func-addrs)
    (mapcar
     #'(lambda (instr) (patch-function-names instr functable))
     (append prelude (append-code-chunks functions)))))

(defun print-code (code &optional (dest t))
  (dotimes (lineno (length code))
    (let ((instr (elt code lineno)))
      (format dest
	      (if (find (first instr)
			(list 'halt 'in 'out 'add 'sub 'mul 'div))
		  "~3D:  ~5S  ~D,~D,~D~40T* ~A~%"
		"~3D:  ~5s  ~D,~D(~D)~40T* ~A~%")
	      lineno
	      (first instr)
	      (second instr)
	      (third instr)
	      (fourth instr)
	      (fifth instr)))))

;; TODO:
;; fix print parsing
;; add (defun generate ((node ast-printop)) ..)
;; optimizations? redundant load, store-load cycles, etc
