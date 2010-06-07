README for Klein semantic analyzer
Chuck Hoffman
Translation of Programing Languages
Fall 2007

As usual, I got started sometime late Sunday afternoon... this time, however, I'll try not to bore you with the long version of the story (it has less twists in it tan the parser story anyway).

Requirements:

* ensure a unique user-defined function names "main"

  This is accomplished directly by type-check-program.  After building the main symbol table (for function definitions), it is searched for such a function.  As to its uniqueness, st-insert will bark if given a symbol already found in the table (this also ensures that a function does not have multiple parameters with the same name).

* ensure that there is no user-defined function called "print"
* ensure that no other reserved word is used as an identifier

  Each reserved word is already given its own special token by the scanner, so any syntactically improper use of a reserved word results in a parser error.

* ensure each function refers only to local data objects

  As I see it, there are no non-local data objects in Klein anyway.  If global variables were to be added to klein, the semantic analyzer will already be be ready for them.  Identifiers declared in functions that are the same as the names of other functions will shadow the function name (causing a different kind of semantic error if the name is subsequently used in a function-call context).

* ensure that al literals satisfy the semantic definition of the language

  I think much of this is already dealt with in the static analysis.  The semantic analyzer infers types for literals (in most cases, the parser already assigned type names to them) and makes sure that they are used in type-consistent context.

* verify all expressions have valid types

  I handled this by writing new CLOS methods for the classes used in the AST.  From there, type checking proceeds mostly by structural recursion.

* detect and report semantic errors

  Type errors will emit messages as they are encountered, but do not halt the type-checking or symbol-table construction (so multiple type errors can be found at one run).  Some other semantic errors (such as the absence of a main method) I have decided to use the (error ...) form for, so the type-checking will be halted.  I could really use better error-handling-fu in Lisp, however :D

* produce a symbol table

  I elected to produce multiple symbol tables -- one for the program at-large that contains entries for sunction definitions, and one for each function definition.  Symbol tables for function definition contain a link to the main symbol table for the program (the "enclosing scope.")

* produce a cross-reference

  I made the cross-reference part of the symbol table.  The print-symbol-table will display all type and cross-reference information.

  For further discussion of the design, see the comments in klein-semantic.lisp.  (Some changes to klein-ast.lisp were also needed along the way).

Running the sucka:

Open klein-demo.lisp.  Modify *klein-dir* as needed, and evaluate the file's contents.  Any type errors in euclid.kl will be displayed, followed by the symbol-table/cross-reference.  I tested it ad-hoc by inserting type errors into euclid-kl and running it :/

