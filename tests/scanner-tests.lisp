;;; scanner-tests.lisp
;;; tests for Klein scanner
;;; Charles Hoffman

(defun test-word-save ()
  (init-with-string "word ")
  (assert (string= (token-text (next-token)) "word")))

(defun test-number-save ()
  (init-with-string "   456")
  (assert (string= (token-text (next-token)) "456")))

(defun test-id-too-long-error ()
  (init-with-string
   (concatenate
    'string
    "id34567890eleven789twenty6789thirty6789fourty6789fifty56789sixty5"
    "6789seventy789eighty6789ninety6789hundred789ten3456789twenty67890"
    "123456789fourty6789fifty56789sixty56789seventy789eighty6789ninety"
    "6789twohundredten3456789twenty6789thirty6789fourty6789fifty56X"))
  (assert (eq (token-type (next-token)) 'err)))

(defun test-token (actual expected)
  (assert (eq (token-type actual) (first expected))
	  ((first actual))
	  (concatenate 'string
		       "Token type "
		       (symbol-name (token-type actual))
		       " not equal to "
		       (symbol-name (first expected))))
  (assert (=  (token-line actual) (second expected))
	  ((third actual))
	  (concatenate 'string
		       "Line number "
		       (write-to-string (token-line actual))
		       " not equal to "
		       (write-to-string (second expected)))))

(defun test-abs-example ()
  (init-with-string
"main( x : integer ) : integer
   if x < 0
   then
      0 - x
   else
      x
   endif")
  (dolist (expected
	   '((sysid-main       1)
	     (lparen           1)
	     (id               1)
	     (colon            1)
	     (typename-integer 1)
	     (rparen           1)
	     (colon            1)
	     (typename-integer 1)
	     (cond-if          2)
	     (id               2)
	     (biop-less        2)
	     (lit-integer      2)
	     (cond-then        3)
	     (lit-integer      4)
	     (biop-minus       4)
	     (id               4)
	     (cond-else        5)
	     (id               6)
	     (cond-endif       7)
	     (eof              7)
	     (eof              7)))
      (test-token (next-token) expected)))


(defun test-others ()
  (init-with-string
   "print true or not false")
  (dolist (expected
	   '((sysid-print   1)
	     (lit-boolean-t 1)
	     (biop-or       1)
	     (unop-not      1)
	     (lit-boolean-f 1)))
    (test-token (next-token) expected)))


(defun run-scanner-tests ()
  (test-word-save)
  (test-number-save)
  (test-id-too-long-error)
  (test-abs-example)
  (test-others))