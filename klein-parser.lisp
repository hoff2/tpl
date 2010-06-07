;;; klein-parser.lisp
;;; Table-driven LL(1) parser for the Klein language
;;; Charles Hoffman
;;; 810:155 Translation of Programming Languages, fall 2007

;;; Removes all instances of the aforementioned symbol from a list/set/sequence
(defun remove-emptys (symbols) (remove 'empty symbols))

(defun contains-empty (symbols) (find 'empty symbols))

;;; Set-equality between two lists
(defun set= (set1 set2)
  (eq nil (set-exclusive-or set1 set2)))

;;; For this parser, the grammar is to be given as a list of rules, where a
;;; rule is a two-element list: the first element is the left side of the
;;; grammar rule, and the second element is the right side, as a nonempty list.
;;; The first element of the grammar list must be a rule for the start symbol.
(defun left-side (rule) (car rule))
(defun right-side (rule) (cadr rule))
(defun is-null-rule (rule) (equalp (right-side rule) (list 'empty)))


; ---------------------------------------------------------------------------
; Much of the parser functionality is closed over the symbols in this let
; so as to not have to pass the values around to everything, but also so that
; they not be global.

(let ((current-grammar nil)            ; the grammar being parsed with
      (semantics nil)                  ; list of semantic tokens
      (consume nil)                    ; function to be called to consume token
      (peek nil)                       ; function to be called to peek token
      (first-memo (make-hash-table))   ; memoization hash for first sets
      (follow-memo (make-hash-table))  ; memoization hash for follow sets
      (table-memo (make-hash-table))   ; memoization hash for parse table
      (debug nil))

  (defun current-grammar () current-grammar)
  (defun semantics () semantics)

  ;;; a token is assumed to be a terminal if it does not appear on the left
  ;;; side of any rule in the grammar
  (defun terminalp (symbol)
    (and (not (find-if
	       #'(lambda (rule) (eq (left-side rule) symbol))
	       current-grammar))
	 (not (semanticp symbol))))

  (defun nonterminalp (symbol)
    (find-if #'(lambda (rule) (eq (left-side rule) symbol)) current-grammar))

  (defun semanticp (symbol)
    (find symbol semantics))
  
  ;;; as stated above, the first rule in the grammar must be a rule for the
  ;;; start symbol
  (defun start-symbolp (symbol)
    (eq symbol (left-side (first current-grammar))))
  
  ;;; returns the rules whose left side matches the argument symbol
  (defun get-rules-for (symbol)
    (remove-if-not
     #'(lambda (rule) (eq (left-side rule) symbol))
     current-grammar))

  ;;; returns the rules in which the argument symbol appears on the right side
  (defun get-rules-producing (symbol)
    (remove-if-not
     #'(lambda (rule) (find symbol (right-side rule)))
     current-grammar))

  ;;; looks for a null rule for the argument symbol
  (defun has-null-rule (symbol)
    (find-if
     #'(lambda (rule) 
	 (and (eq (left-side rule) symbol)
	      (is-null-rule rule)))
     current-grammar))

  ;;; removes any symbols in the semantic tokens list from the right side of
  ;;; a rule.  This is necessary for computing first and follow sets.
  (defun remove-semantics (rule)
    (list (left-side rule)
	  (remove-if
	   #'(lambda (symbol) (find symbol semantics))
	   (right-side rule))))


  (defun first-set (symbol)
    (or (gethash symbol first-memo)
	(labels
	    ((firsts-from-production
	      (prod)
	      (if (null prod) (list 'empty)
		(union (remove-emptys (first-set (first prod)))
		       (if (has-null-rule (first prod))
			    (firsts-from-production (rest prod))))))
	     (firsts-from-rule-list
	      (rules)
	      (if (not (null rules))
		  (union (if (is-null-rule (first rules))
			     (list 'empty)
			   (firsts-from-production (right-side (first rules))))
			 (firsts-from-rule-list (rest rules))))))
	  (setf (gethash symbol first-memo)
		(if (terminalp symbol)
		    (list symbol)
		  (firsts-from-rule-list
		   (mapcar #'remove-semantics (get-rules-for symbol))))))))


  (defun follow-set (symbol &optional (exclude nil))
    (or (gethash symbol follow-memo)
	(labels
	    ((last-symbol (prod) (first (last prod)))
	     (second-to-last-symbol (prod) (first (last (butlast prod))))
	     (symbols-following
	      (symbol prod)
	      (if (not (or (null prod) (= 1 (length prod))))
		  (union (if (eq symbol (first prod)) (list (second prod)))
			 (symbols-following symbol (rest prod)))))
	     (collect-firsts
	      (symbols)
	      (if (not (null symbols))
		  (union (remove-emptys (first-set (first symbols)))
			 (collect-firsts (rest symbols)))))
	     (can-end-rule
	      (symbol rule)
	      (let ((prod (right-side rule)))
		(or (eq symbol (last-symbol prod))
		    (and (eq symbol (second-to-last-symbol prod))
			 (contains-empty (first-set (last-symbol prod)))))))
	     (follows-from-rule
	      (rule exclude)
	      (let ((prod (right-side rule)))
		(union (collect-firsts (symbols-following symbol prod))
		       (if (can-end-rule symbol rule)
			   (follow-set (left-side rule)
				       (adjoin symbol exclude))))))
	     (follows-from-rule-list
	      (rules exclude)
	      (if (not (null rules))
		  (union (follows-from-rule (first rules) exclude)
			 (follows-from-rule-list (rest rules) exclude)))))
	  (setf (gethash symbol follow-memo)
		(if (not (or (terminalp symbol) (find symbol exclude)))
		    (union (if (start-symbolp symbol) (list 'eof))
			   (follows-from-rule-list
			    (mapcar #'remove-semantics 
				    (get-rules-producing symbol))
			    exclude)))))))


  (defun parse-table (symbol terminal)
    (or (gethash (list symbol terminal) table-memo)
	(flet
	    ((parse-entry-from-rule
	      (terminal rule)
	      (let ((production (right-side (remove-semantics rule))))
		(if (or (find terminal (first-set (first production)))
			(and (find 'empty
				   (first-set (first production)))
			     (find terminal (follow-set (left-side rule)))))
		  rule))))
	  (setf (gethash (list symbol terminal) table-memo)
		(remove-if #'null
			   (mapcar #'(lambda (rule)
				       (parse-entry-from-rule terminal rule))
				   (get-rules-for symbol)))))))


  ;; This function initializes the parser.  It takes the following keyword
  ;; arguments:
  ;;
  ;; :grammar       The LL(1) grammar to parse with.  This is in the form of a
  ;;                list of rules, with the first rule being a rule for the
  ;;                start symbol.  Each rule should be a two-element list with a
  ;;                nonterminal symbol (left side) as the first element and a
  ;;                nonempty list of symbols (the production, or right side) as
  ;;                the second.  The production may contain semantic action
  ;;                symbols if they are all listed in the next argument, and for
  ;;                null rules must use a list containing the symbol 'empty.
  ;;
  ;; :ast-types     A list of symbols that should be interpreted as semantic
  ;;                actions, rather than as grammar symbols.  For each of these
  ;;                there should be a class of the same name defined from which
  ;;                to instantiate AST nodes.
  ;;
  ;; :consume-func  A function from which the parser can consume tokens.  The
  ;;                scanner should produce a token with type symbol 'eof at the
  ;;                end of program.
  ;;
  ;; :peek-func     A function from which the parser can peek at the next token.
  ;;
  ;; :print         Optional -- print messages about parse's state as it goes
  ;;
  (defun setup-parser
    (&key grammar ast-types consume-func peek-func (print nil))
    (setf current-grammar grammar)
    (setf semantics ast-types)
    (setf consume consume-func)
    (setf peek peek-func)
    (setf debug print)
    (clrhash first-memo)
    (clrhash follow-memo)
    (clrhash table-memo))

  (defun unsetup-parser ()
    (setup-parser :grammar nil
		  :ast-types nil
		  :consume-func nil
		  :peek-func nil))

  (defun parse (&optional (current-token nil)
			  (parse-stack
			   (list (left-side (first current-grammar)) 'eof))
			  (semantic-stack nil))
    (let* ((a (car parse-stack))
	   (itok (funcall peek))
	   (i (token-type itok)))
      (if debug
	  (format t "~&parse: ~S~%peek: ~S~%current: ~S~%pst: ~S~%sst:~S~%~%"
		  a itok current-token parse-stack semantic-stack))
      (if (eq a 'eof) (car semantic-stack)
	(cond ((terminalp a)
	       (if (eq a i)
		   (parse (funcall consume)
			  (cdr parse-stack)
			  semantic-stack)
		 (error "~S (~A) at line ~D: ~S != ~S"
			(token-type itok)
			(token-text itok)
			(token-line itok) a i)))
	      ((nonterminalp a)
	       (let ((rules (parse-table a i)))
		 (cond ((null rules)
			(error "~S (~A) at line ~D: no rule (~S, ~S)"
			       (token-type itok)
			       (token-text itok)
			       (token-line itok) a i))
		       ((> 1 (length rules))
			(error "Multiple rules (~S, ~S): ~S" a i rules))
		       (t 
			(parse current-token
			       (append
				(remove-emptys (right-side (first rules)))
				(cdr parse-stack))
			       semantic-stack)))))
	      ((semanticp a)
	       (let ((node (make-instance a)))
		 (parse current-token
			(cdr parse-stack)
			(build node semantic-stack current-token))))
	      (t (error "Invalid token ~S" itok))))))
  ) ;let
