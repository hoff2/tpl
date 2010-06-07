(defparameter *klein-semantics*
  '(ast-literalint
    ast-literalboolt
    ast-literalboolf
    ast-identifier
    ast-typeint
    ast-typebool
    ast-call
    ast-ifexpr
    ast-lessop
    ast-equalsop
    ast-orop
    ast-plusop
    ast-minusop
    ast-timesop
    ast-divideop
    ast-printop
    ast-notop
    ast-actual
    ast-formal
    ast-definition
    ast-actualseq
    ast-formalseq
    ast-definitionseq
    ast-program))

(defparameter *klein-grammar-semantic*
  '((program          (ast-definitionseq definitions ast-program))
    
    (definitions      (def definitionsp))
    (definitionsp     (definitions))
    (definitionsp     (empty))
    (def              (identifier lparen ast-formalseq formals rparen
			  colon type body ast-definition))
    
    (formals          (empty))
    (formals          (nonemptyformals))
    (nonemptyformals  (formal nonemptyformalsp))
    (nonemptyformalsp (comma nonemptyformals))
    (nonemptyformalsp (empty))
    (formal           (identifier colon type ast-formal))
    
    (body             (sysid-print body ast-printop))
    (body             (expr))
    
    (type             (typename-integer ast-typeint))
    (type             (typename-boolean ast-typebool))
    
    (expr             (term exprp))
    (exprp            (biop-less term ast-lessop exprp))
;    (exprp            (biop-equal term ast-equalsop exprp))
    (exprp            (biop-equal term exprp ast-equalsop))
    (exprp            (empty))
    
    (term             (atom termp))
;    (termp            (biop-or atom ast-orop termp))
    (termp            (biop-or atom termp ast-orop))
    (termp            (biop-plus atom ast-plusop termp))
    (termp            (biop-minus atom ast-minusop termp))
    (termp            (biop-times atom ast-timesop termp))
    (termp            (biop-divide atom ast-divideop termp))
    (termp            (empty))
    
    (atom             (cond-if expr
		       cond-then expr
		       cond-else expr
		       cond-endif ast-ifexpr))
    (atom             (unop-not expr ast-notop))
    (atom             (identifier atomp))
    (atom             (literal))
    (atomp            (lparen ast-actualseq actuals rparen ast-call))
    (atomp            (empty))

    (identifier       (id ast-identifier))
    (identifier       (sysid-main ast-identifier))

    (literal          (lit-integer ast-literalint))
    (literal          (lit-boolean-t ast-literalboolt))
    (literal          (lit-boolean-f ast-literalboolf))
    
    (actuals          (empty))
    (actuals          (nonemptyactuals))
    (nonemptyactuals  (expr ast-actual nonemptyactualsp))
    (nonemptyactualsp (comma nonemptyactuals))
    (nonemptyactualsp (empty))))
