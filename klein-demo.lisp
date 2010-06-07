; Change this value to the path where the klein folder is for you
; then evaluate this form
(defparameter *klein-dir* "g:/155/klein/")
(defparameter *test-ast* nil)

(load (concatenate 'string *klein-dir* "klein-scanner.lisp"))
(load (concatenate 'string *klein-dir* "klein-ast.lisp"))
(load (concatenate 'string *klein-dir* "klein-parser.lisp"))
(load (concatenate 'string *klein-dir* "klein-grammar.lisp"))
(load (concatenate 'string *klein-dir* "klein-semantic.lisp"))
(load (concatenate 'string *klein-dir* "klein-tiny.lisp"))

(defun klein-load (filename)
  (setup-scanner-with-file filename)
  (setup-parser :grammar      *klein-grammar-semantic*
		:ast-types    *klein-semantics*
		:consume-func #'next-token
		:peek-func    #'peek-token
		:print        nil))

(defun klein-compile (name &optional (use-klein-dir nil))
  (let* ((dir     (if use-klein-dir *klein-dir* ""))
	 (infile  (concatenate 'string dir name ".kln"))
	 (outfile (concatenate 'string dir name ".tm")))
    (klein-load infile)
    (let ((ast (parse)))
      (type-check-program ast) ; necessary because also generates symbol tables
      (with-open-file
       (of outfile  :direction :output :if-does-not-exist :create)
       (print-code (generate ast) of)))))

