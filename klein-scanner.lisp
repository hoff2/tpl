;;; klein-scanner.lisp
;;; Lexical scanner for the Klein language
;;; Charles Hoffman
;;; 810:155 Translation of Programming Languages, fall 2007

(defparameter *line-count* 1)

; ----------------------------------------------------------------------------
;; working with the input stream

;; consume a character (will return space in lieu of end-of-file)
(defun consume (strm)
  (read-char strm nil #\Space))

;; Peek at the next character, (or return space in lieu of end-of-file)
(defun peek (strm)
  (peek-char nil strm nil #\Space))

;; End-of-file should be checked for first by the start state
(defun eof-p (strm)
  (eq (peek-char nil strm nil 'eof) 'eof))

; ---------------------------------------------------------------------------
;; useful predicates for characters

;; is it a whitespace character?
(defun whitespace-char-p (char)
  (or (char= char #\Space)
      (char= char #\Tab)
      (char= char #\Newline)
      (char= char #\Return)))

;; is it a nonzero digit?
(defun nonzero-digit-p (char)
  (and (digit-char-p char)
       (char/= char #\0)))

; ----------------------------------------------------------------------------
;; tokens are represented by a three-element list
(defun make-token (type &optional (text "") (line *line-count*))
  (list type text line))

;; the first element of a token is a symbol denoting the type of token
(defun token-type (tok)
  (car tok))

;; the second element is optionally the text that was scanned to produce the
;; token, or when that's pretty well implied, can be an empty string
(defun token-text (tok)
  (cadr tok))

;; the third element is the line number at which the token was identified
(defun token-line (tok)
  (caddr tok))

; ----------------------------------------------------------------------------
;; storing up text values of tokens (or willfully neglecting to do so)

;; when I get a token back from the next state, remember that this character
;; gets appended at the beginning of its text value (thus the text element of a
;; token will be built recursively)
(defun remember (char tok)
  (make-token (token-type tok)
	      (concatenate 'string (string char) (token-text tok))
	      (token-line tok)))

;; used for "word" tokens to make sure that an err token will be issued if a
;; word is found to be over 256 characters long.
(defun remember-word (char tok)
  (if (>= (length (token-text tok)) 256)
      (make-token 'err "Identifier too long")
    (remember char tok)))


;; remember's evil twin, ignores the char and leaves the token as is
(defun forget (char tok)
  (declare (ignore char)) tok)

; ----------------------------------------------------------------------------
;; Functions representing DFA states.  State transisition is performed by
;; calling another state-function; "final" (longest-match) acceptance is
;; done by returning a value (a token)

(defun start-state (strm)
  ; only the start state should see end-of-file
  (if (eof-p strm)
      (make-token 'eof)
    (let ((char (consume strm)))
      (if (char= char #\Newline) (incf *line-count*))
      (cond
       ((eq char 'eof)           (make-token 'eof))
       ; whitespace is significant only as a longest-match delimiter
       ; for certain other states
       ((whitespace-char-p char) (start-state strm))
       ; meaningful first-characters:
       ((eq char #\+)            (make-token 'biop-plus))
       ((eq char #\-)            (make-token 'biop-minus))
       ((eq char #\*)            (make-token 'biop-times))
       ((eq char #\/)            (slash-state strm))
       ((eq char #\<)            (make-token 'biop-less))
       ((eq char #\=)            (make-token 'biop-equal))
       ((eq char #\()            (make-token 'lparen))
       ((eq char #\))            (make-token 'rparen))
       ((eq char #\:)            (make-token 'colon))
       ((eq char #\,)            (make-token 'comma))
       ((eq char #\0)            (remember char (zero-state strm)))
       ((nonzero-digit-p char)   (remember char (number-state strm)))
       ((alpha-char-p char)      (remember-word char (word-state strm)))
       ; any other character not previously mentioned is invalid syntax here
       (t                        (make-token 'err
					     (concatenate
					      "Unexpected character: "
					      (string char))))))))

;; If we have seen a slash, check whether another slash immediately follows
;; (signaling a comment), or not (the slash was a divide operator)
(defun slash-state (strm)
  (let ((char (consume strm)))
    (cond
     ((eq char #\/)            (comment-state strm))
     (t                        (make-token 'biop-divide)))))

;; If we are in a comment, "forget" characters until a newline approaches.
(defun comment-state (strm)
  (if (eq (peek strm) #\Newline)
      (start-state strm)
    (forget (consume strm) (comment-state strm))))

;; A single 0 is the numeric literal for zero; if other digits immediately
;; follow, it is a leading zero which is not allowed.
(defun zero-state (strm)
  (if (digit-char-p (peek strm))
      (make-token 'err "Numeric literal with leading zero")
    (make-token 'lit-integer)))

;; Accept when we have the rest of this numeric literal
(defun number-state (strm)
  (if (not (digit-char-p (peek strm)))
      (make-token 'lit-integer)
    (remember (consume strm) (number-state strm))))

;; A "word" token is an intermediate token that will be resolved later to be
;; either a reserved word or identifier.
(defun word-state (strm)
  (if (not (alphanumericp (peek strm)))
      (make-token 'word)
    (remember-word (consume strm) (word-state strm))))

; ----------------------------------------------------------------------------
;; Intended for resolving "word" tokens into another kind of token depending on
;; whether the text matches a reserved word -- the value found in the hash table
;; will be the token's new type.  If not found, the token is considered an
;; identifier.
(defun reserved-lookup (tok wordhash)
  (make-token
   (or (gethash (token-text tok) wordhash) 'id)
   (token-text tok)
   (token-line tok)))

;; Build a hash table of all of Klein's reserved words and the token-types to
;; give for each.
(defun make-reserved-hash ()
  (let ((words (make-hash-table :size 12 :test 'equal)))
    (setf (gethash "true"    words) 'lit-boolean-t)
    (setf (gethash "false"   words) 'lit-boolean-f)
    (setf (gethash "integer" words) 'typename-integer)
    (setf (gethash "boolean" words) 'typename-boolean)
    (setf (gethash "not"     words) 'unop-not)
    (setf (gethash "or"      words) 'biop-or)
    (setf (gethash "if"      words) 'cond-if)
    (setf (gethash "then"    words) 'cond-then)
    (setf (gethash "else"    words) 'cond-else)
    (setf (gethash "endif"   words) 'cond-endif)
    (setf (gethash "main"    words) 'sysid-main)
    (setf (gethash "print"   words) 'sysid-print)
    words))

;; ---------------------------------------------------------------------------
;; Main program of scanner: first initialize with a stream, then call
;; next-token until you receive an eof token.
(let ((input nil)
      (reserved nil)
      (peekachu nil))

  (defun setup-scanner (strm)
    (setf *line-count* 1)
    (setf reserved (make-reserved-hash))
    (setf input strm)
    (setf peekachu nil))

  (defun debug-peek ()
    (peek input))

  (defun next-token ()
    (if peekachu
	(let ((next-token peekachu))
	  (setf peekachu nil)
	  next-token)
      (let ((tok (start-state input)))
	(if (eq (token-type tok) 'word)
	    (reserved-lookup tok reserved)
	  tok))))

  (defun peek-token ()
    (or peekachu
	(setf peekachu (next-token))
      peekachu)))

(defun setup-scanner-with-string (str)
  (setup-scanner (make-string-input-stream str)))

(defun setup-scanner-with-file (filename)
  (setup-scanner (open filename)))
