#lang pl 04

#| BNF for the ALGAE language:
     <ALGAE> ::= <num>
               | True
               | False
               | { + <ALGAE> ... }
               | { * <ALGAE> ... }
               | { - <ALGAE> <ALGAE> ... }
               | { / <ALGAE> <ALGAE> ... }
               | { <  <ALGAE> <ALGAE> }
               | { =  <ALGAE> <ALGAE> }
               | { <= <ALGAE> <ALGAE> }
               | { if <ALGAE> <ALGAE> <ALGAE> }
               | { not <ALGAE> }
               | { and <ALGAE> <ALGAE> }
               | { or <ALGAE> <ALGAE> }
               | { with { <id> <ALGAE> } <ALGAE> }
               | <id>
|#

;; ALGAE abstract syntax trees
(define-type ALGAE
  [Num  Number]
  [Bool Boolean]
  [Add  (Listof ALGAE)]
  [Mul  (Listof ALGAE)]
  [Sub  ALGAE (Listof ALGAE)]
  [Div  ALGAE (Listof ALGAE)]
  [Less ALGAE ALGAE]
  [Equal ALGAE ALGAE]
  [LessEq ALGAE ALGAE]
  [If ALGAE ALGAE ALGAE]
  [Id   Symbol]
  [With Symbol ALGAE ALGAE])

(: parse-sexpr : Sexpr -> ALGAE)
;; parses s-expressions into ALGAEs
(define (parse-sexpr sexpr)
  ;; utility for parsing a list of expressions
  (: parse-sexprs : (Listof Sexpr) -> (Listof ALGAE))
  (define (parse-sexprs sexprs) (map parse-sexpr sexprs))
  (match sexpr
    [(number: n)    (Num n)]
    ['True  (Bool #t)]
    ['False (Bool #f)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ args ...)     (Add (parse-sexprs args))]
    [(list '* args ...)     (Mul (parse-sexprs args))]
    [(list '- fst args ...) (Sub (parse-sexpr fst) (parse-sexprs args))]
    [(list '/ fst args ...) (Div (parse-sexpr fst) (parse-sexprs args))]
    [(list '< l r)  (Less (parse-sexpr l) (parse-sexpr r))]
    [(list '= l r)  (Equal (parse-sexpr l) (parse-sexpr r))]
    [(list '<= l r) (LessEq (parse-sexpr l) (parse-sexpr r))]
    [(list 'if cond then-val else-val)
     (If (parse-sexpr cond) (parse-sexpr then-val) (parse-sexpr else-val))]
    [(list 'not arg) (Not (parse-sexpr arg))]
    [(list 'and arg1 arg2) (And (parse-sexpr arg1) (parse-sexpr arg2))]
    [(list 'or arg1 arg2) (Or (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: Not : ALGAE -> ALGAE)
;; bind Not to the actual ALGAE syntax
(define (Not expr)
  (If expr (Bool #f) (Bool #t)))

(: Or : ALGAE ALGAE -> ALGAE)
;; bind Or to the actual ALGAE syntax
(define (Or expr1 expr2)
  (If expr1 (Bool #t) expr2))

(: And : ALGAE ALGAE -> ALGAE)
;; bind And to the actual ALGAE syntax
(define (And expr1 expr2)
  (If expr1 expr2 (Bool #f)))

(: parse : String -> ALGAE)
;; parses a string containing an ALGAE expression to an ALGAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `B' is a True/False
   `E1', `E2' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      B[v/x]                = B
      {+ E ...}[v/x]        = {+  E[v/x] ...}
      {* E ...}[v/x]        = {*  E[v/x] ...}
      {- E1 E ...}[v/x]     = {-  E1[v/x] E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/  E1[v/x] E[v/x] ...}
      {< E1 E2}[v/x]        = {<  E1[v/x] E2[v/x]}
      {= E1 E2}[v/x]        = {=  E1[v/x] E2[v/x]}
      {<= E1 E2}[v/x]       = {<= E1[v/x] E2[v/x]}
      {if E1 E2 E3}[v/x]    = {if E1[v/x] E2[v/x] E3[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#

(: subst : ALGAE Symbol ALGAE -> ALGAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  ;; convenient helper -- no need to specify `from' and `to'
  (: subst* : ALGAE -> ALGAE)
  (define (subst* x) (subst x from to))
  ;; helper to substitute lists
  (: substs* : (Listof ALGAE) -> (Listof ALGAE))
  (define (substs* exprs) (map subst* exprs))
  (cases expr
    [(Num n)        expr]
    [(Bool b)       expr]
    [(Add args)     (Add (substs* args))]
    [(Mul args)     (Mul (substs* args))]
    [(Sub fst args) (Sub (subst* fst) (substs* args))]
    [(Div fst args) (Div (subst* fst) (substs* args))]
    [(Less l r)   (Less   (subst* l) (subst* r))]
    [(Equal l r)  (Equal  (subst* l) (subst* r))]
    [(LessEq l r) (LessEq (subst* l) (subst* r))]
    [(If cond then else)
     (If (subst* cond) (subst* then) (subst* else))]
    [(Id name)      (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst* named-expr)
           (if (eq? bound-id from)
               bound-body
               (subst* bound-body)))]))

#| Formal specs for `eval':
     eval(N)             = N
     eval(B)             = B
     eval({+ E ...})     = evalN(E) + ...
     eval({* E ...})     = evalN(E) * ...
     eval({- E})         = -evalN(E)
     eval({/ E})         = 1/evalN(E)
     eval({- E1 E ...})  = evalN(E1) - (evalN(E) + ...)
     eval({/ E1 E ...})  = evalN(E1) / (evalN(E) * ...)
     eval({< E1 E2})     = evalN(E1) <  evalN(E2)
     eval({= E1 E2})     = evalN(E1) =  evalN(E2)
     eval({<= E1 E2})    = evalN(E1) <= evalN(E2)
     eval({if E1 E2 E3}) = eval(E2) if evalB(E1) is true
                         = eval(E3) otherwise
     eval({not E})       = eval({if E False True})
     eval({or E1 E2})    = eval({if E1 True E2})
     eval({and E1 E2})   = eval({if E1 E2 False})
     eval(id)            = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     evalN(E) = eval(E) if it is a number, error otherwise
     evalB(E) = eval(E) if it is a boolean, error otherwise
|#

(: eval-number : ALGAE -> Number)
;; helper for `eval': verifies that the result is a number
(define (eval-number expr)
  (let ([result (eval expr)])
    (if (number? result)
        result
        (error 'eval-number "need a number when evaluating ~s, but got ~s"
               expr result))))

(: eval-boolean : ALGAE -> Boolean)
;; helper for `eval': verifies that the result is a boolean
(define (eval-boolean expr)
  (let ([result (eval expr)])
    (if (boolean? result)
        result
        (error `eval-boolan "need a boolean when evaluating ~s, but got ~s"
               expr result))))

(: value->algae : (U Number Boolean) -> ALGAE)
;; converts a value to an ALGAE value (so it can be used with `subst')
(define (value->algae val)
  (cond [(number? val) (Num val)]
        [(boolean? val) (Bool val)]
        ;; Note: a `cond' doesn't make much sense now, but it will when
        ;; we extend the language with booleans.  Also, since we use
        ;; Typed Racket, the type checker makes sure that this function
        ;; is never called with something that is not in its type, so
        ;; there's no need for an `else' branch like
        ;;     [else (error 'value->algae "unexpected value: ~s" val)]
        ;; (Strictly speaking, there's no need for the last predicate
        ;; (which is the only one here until you extend this), but it's
        ;; left in for clarity.)
        ))

(: eval : ALGAE -> (U Number Boolean))
;; evaluates ALGAE expressions by reducing them to numbers
(define (eval expr)
  ;; map list of algae to list of numbers
  (: map-nums : (Listof ALGAE) -> (Listof Number))
  (define (map-nums args)
    (map eval-number args))
  (cases expr
    [(Num n) n]
    [(Bool b) b]
    [(Add args) (foldl + 0 (map-nums args))]
    [(Mul args) (foldl * 1 (map-nums args))]
    [(Sub fst args) (let ([first-num (eval-number fst)])
                      (if (null? args)
                          (- 0 first-num)
                          (- first-num (foldl + 0 (map-nums args)))))]
    [(Div fst args) (let ([first-num (eval-number fst)]
                          [rest-mul (foldl * 1 (map-nums args))])
                      (cond
                        [(zero? (or (and (null? args) first-num) rest-mul))
                         (error 'eval "attemps a division by zero")]
                        [(null? args)
                         (/ first-num)]
                        [else (/ first-num (foldl * 1 (map-nums args)))]))]
    [(Less l r)   (< (eval-number l) (eval-number r))]
    [(Equal l r)  (= (eval-number l) (eval-number r))]
    [(LessEq l r) (<= (eval-number l) (eval-number r))]
    [(If cond then else) (eval (if (eval-boolean cond) then else))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  ;; see the above `value->algae' helper
                  (value->algae (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (U Number Boolean))
;; evaluate an ALGAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests (for simple expressions)
(test (run "5") => 5)
(test (run "{+ 5 5}") => 10)
(test (run "{with {x {+ 5 5}} {+ x x}}") => 20)
(test (run "{with {x 5} {+ x x}}") => 10)
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => 14)
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => 4)
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => 15)
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => 8)
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => 10)
(test (run "{with {x 5} {with {y x} y}}") => 5)
(test (run "{with {x 5} {with {x x} x}}") => 5)
(test (run "{with {x {+ 5 5}} {* x x}}") => 100)
(test (run "{with {x {+ 5 5}} {/ x x}}") => 1)
(test (run "{with {x} {* x x}}") =error> "bad `with' syntax in")
(test (run "{with {x 5} {? x x}}") =error> "bad syntax in")
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{with {x 1} {/ x x x x}}") => 1)
(test (run "{with {x 2} {/ x}}") => 1/2)
(test (run "{with {x 0} {/ x}}") =error> "attemps a division by zero")
(test (run "{with {x 0} {with {y 3} {/ y y x}}}")
      =error> "attemps a division by zero")
(test (run "{with {x 1} {- x}}") => -1)
(test (run "{with {x 1} {with {y 3} {- x y x y}}}") => -6)
(test (run "{with {x 1} {with {y 3} {* x y x y}}}") => 9)
(test (run "{with {x 1} {with {y 3} {/ x y x y}}}") => 1/9)
(test (run "{with {x 1} {with {y 3} {+ x y x y}}}") => 8)
(test (run "{+}") => 0)
(test (run "{*}") => 1)
(test (run "{with {x 1} {- x {< 1 3}}}")
      =error> "need a number when evaluating")
(test (run "{< 1 3}"))
(test (not (run "{= 1 3}")))
(test (run "{= 3 3}"))
(test (run "{<= 3 3}"))
(test (run "True"))
(test (not (run "False")))
(test (run "{with {x {<= 3 3}} {if x 1 4}}") => 1)
(test (run "{with {x 4} {with {y 6} {< x y}}}"))
(test (run "{with {x 4} {= x x}}"))
(test (run "{with {x 4} {<= x x}}"))
(test (run "{with {x {= 4 3}} {if x 1 4}}") => 4)
(test (run "{with {x 1} {if x 1 0}}") =error> "need a boolean when evaluating")
(test (run "{with {x True} {if x True False}}"))
(test (not (run "{and True False}")))
(test (run "{and True True}"))
(test (run "{or False True}"))
(test (run "{or True False}"))
(test (run "{or True {/ 1 0}}"))
(test (run "{or {/ 1 0} True}") =error> "attemps a division by zero")
(test (not (run "{and False {/ 1 0}}")))
(test (not (run "{not {< 1 3}}")))
(test (run "{not {< 3 1}}"))
(test (run "{and {or {= 1 1} {< 3 1}} True}"))

(define minutes-spent 250)