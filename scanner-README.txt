Charles Hoffman
Translation of Programming Languages, Fall 2007
Project part 1: Scanner

The design of my scanner is basically DFA-based with a few ad-hoc pragmatic touches and some ideas borrowed from the TINY scanner in Louden's book.  Each state of the DFA is represented by a Lisp function that may return either a token or simply call the next state-function.

Three functions deal with the input stream: consume, peek, and eof-p.  eof-p is used to detect the end of the file; the scanner is designed such that this check is made first by the start state before proceeding further; if either consume or peek reach the end of the file, they return a space character instead, in order to prevent the scanner crashing with and end-of-file error before completely consuming the last token (since space is sufficient to delimit pretty much anything).

A couple of predicate functions about characters (whitespace-char-p and nonzero-digit-p) are included for the sake of convenience and conciseness elsewhere.

Tokens are represented by a list having three elements: the first, a symbol identifying the token type; second, the textual value of the token; this is only used in the case of numeric literals and "word tokens" (more on these in a moment).  The third element is the line number at which the scanner identified the token.  Construction and accessor functions are provided for tokens and their elements.

Besides tokens denoting language elements, the scanner may also produce the special bookkeeping token types eof and err.  In an err token, the text element will contain an error message.

To save the textual content of tokens, there are three functions used by the DFA states.  The remember function appends the current character to the beginning of the text element of a token; thus a series of deferred calls to remember builds up until an actual token is constructed, initially with an empty string as its text element.  Then as the stack of remember calls unwinds, the text of the token is reconstructed.

remember-word is merely remember with a length check to enforce the length limit on identifiers.  The scanner will read to the end of an overlong identifier, but remember-word will bomb out the reconstruction of its text after the (ending) 256 characters, yielding an err token rather than a word token.

forget is intended simply to ignore the character just read and return the token it is given, as-is.  This is used by the DFA state that recognizes comments.  This mostly just allows comment-state to continue consuming characters and then recursively calling itself, without having to wrap the two sequenced operations up in a progn form.  As a fringe benefit, it makes it easy to modify comment-state to remember comments and issue coment tokens, should we want to do something like that.

As of this writing, I do not have handling for numeric literal tokens to enforce the range of valid values given in the specification.  This functionality is currently missing; I am trying to work out a design for it that will operate in a manner consistent with remember-word.  For now, the scanner uses the remember function for numbers and simply remembers them as text.

start-state -- the function representing the start state of the scanner DFA -- first checks for end-of-file, and issues an eof token if applicable.  Since eof-p does not "consume" the end of file, subsequent calls to next-token will simply yield yet more eof tokens.  Start-state also ignores whitespace and increments the line-counter as needed.  Apart from that, it acts like a normal DFA state, capable of producing "err" tokens and tokens for single-character cases, or calling out to the next state.

There are no paths through the DFA for recognizing reserved words of the language; instead, one path through the DFA recognizes "words" -- a letter followed by letters or digits -- and builds a word token.  Word tokens are then checked against a hash table of reserved words after the DFA produces them; if their text content matches a reserved word in the table, they are converted to a token specific to the reserved word; otherwise they are converted to id tokens, which denote identifiers.  In either case their text content is preserved, though it may be a good idea to instead throw away the text of reserved-word tokens, to conserve memory.

The main interface to the scanner consists of two functions: initialize and next-token.  initialize is used to get the scanner started, and takes a character stream as an argument; next-token produces the next recognized token.

For convenience, two alternate initialization functions are provided: init-with-file takes as an argument the pathname of an input file, while init-with-string allows initializing the scanner from a string, which is useful for testing.

The file scanner-tests.lisp contains a series of test routines for the scanner; I don't know if I'd call this set of tests conprehensive, but in combination with some ad-hoc testing at the REPL it has helped me to be quite confident in the workings of the scanner.  Another file, scanner-demo.lisp, contains the demo function which loads up the scanner and tests, runs the tests, and then performs the requested scanning of a file (euclid.kl in the samples folder) while printing out the tokens.

Comments on the design and/or style much appreciated.