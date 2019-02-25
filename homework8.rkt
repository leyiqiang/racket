;; ** The BRANG interpreter, using environments

#lang pl 08
#|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> <id> ... } <BRANG> }
            | { call <BRANG> <BRANG> ... }
            | { rec { <id> <BRANG> } <BRANG> }

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(x,env)                = lookup(x,env)
  eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
  eval({fun {x} E},env)      = <{fun {x} E}, env>
  eval({call E1 E2},env1)
           = eval(Ef,extend(x,eval(E2,env1),env2))
                             if eval(E1,env1) = <{fun {x} Ef}, env2>
           = error!          otherwise
  eval({rec {x E1} E2}, env) = eval({with {id {call Y {fun {id} E1}}} E2}, env)
|#

(define-type BRANG
  [Num  Number]
  [Add  BRANG BRANG]
  [Sub  BRANG BRANG]
  [Mul  BRANG BRANG]
  [Div  BRANG BRANG]
  [Id   Symbol]
  [With Symbol BRANG BRANG]
  [Fun  (Listof Symbol) BRANG]
  [Call BRANG (Listof BRANG)]
  [WRec Symbol BRANG BRANG])

(define-type CORE
  [CNum  Number]
  [CAdd  CORE CORE]
  [CSub  CORE CORE]
  [CMul  CORE CORE]
  [CDiv  CORE CORE]
  [CRef  Natural]
  [CFun  CORE]
  [CCall CORE CORE])

(: parse-sexpr : Sexpr -> BRANG)
;; parses s-expressions into BRANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name) (symbol: names)...) body)
        (Fun (cons name names) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(cons 'call more)
     (match sexpr
       [(list 'call fun arg args ...)
        (Call (parse-sexpr fun) (map parse-sexpr (cons arg args)))]
       [else (error 'parse-sexpr "bad `call syntax in ~s" sexpr)])]
    [(cons 'rec more)
     (match sexpr
       [(list 'rec (list (symbol: name) named) body)
        (let ([parse-named (parse-sexpr named)])
          (cases parse-named
            [(Fun ids bound-body) (WRec name (parse-sexpr named) (parse-sexpr body))]
            [else (error 'parse-sexpr "non-fun form in `rec'")]))]
       [else (error 'parse-sexpr "bad `rec syntax in ~s" sexpr)])]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> BRANG)
;; parses a string containing a BRANG expression to a BRANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV = (Listof VAL))

;; The type definition of DE-ENV
(define-type DE-ENV = Symbol -> Natural)

;; de-empty-env is the empty environment that has
;; zero mapping in the list
(: de-empty-env : DE-ENV)
(define (de-empty-env symbol)
  (error 'de-empty-env "The environment has no mapping for ~s" symbol))

(: de-extend : DE-ENV Symbol -> DE-ENV)
;; de-extend consumes a DE-ENV and a symbol
;; and returns the extended environment
(define (de-extend env symbol)
  (lambda (s)
    (if (equal? s symbol) 0
        (+ 1 (env s)))))

(define-type VAL
  [NumV Number]
  [FunV CORE ENV])

(: NumV->number : VAL -> Number)
;; convert a BRANG runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : CORE ENV -> VAL)
;; evaluates BRANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(CNum n) (NumV n)]
    [(CAdd l r) (arith-op + (eval l env) (eval r env))]
    [(CSub l r) (arith-op - (eval l env) (eval r env))]
    [(CMul l r) (arith-op * (eval l env) (eval r env))]
    [(CDiv l r) (arith-op / (eval l env) (eval r env))]
    [(CRef natural) (list-ref env natural)]
    [(CFun bound-body)
     (FunV bound-body env)]
    [(CCall fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-body f-env)
          (eval bound-body
                (cons (eval arg-expr env) f-env))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

;; The make-recursive function
;; (define (make-recursive f)
;;  ((lambda (x) (x x))
;;   (lambda (x) (f (lambda (n) ((x x) n))))))
(define Y-combinator (parse "{fun {f}
                               {call {fun {x} {call x x}}
                                     {fun {x}
                                       {call f {fun {n}
                                         {call {call x x} n}}}}}"))
(: preprocess : BRANG DE-ENV -> CORE)
;; translates a given BRANG value to the corresponding CORE value
(define (preprocess expr de-env)
  (cases expr
    [(Num n) (CNum n)]
    [(Add l r) (CAdd (preprocess l de-env) (preprocess r de-env))]
    [(Sub l r) (CSub (preprocess l de-env) (preprocess r de-env))]
    [(Mul l r) (CMul (preprocess l de-env) (preprocess r de-env))]
    [(Div l r) (CDiv (preprocess l de-env) (preprocess r de-env))]
    [(With id named-expr bound-body)
     (CCall (preprocess (Fun (list id) bound-body) de-env)
            (preprocess named-expr de-env))]
    [(Id s) (CRef (de-env s))]
    [(Fun ids bound-body)
     (if (= (length ids) 1)
         (CFun (preprocess bound-body (de-extend de-env (first ids))))
         (preprocess (Fun (list (first ids))
                          (Fun (rest ids) bound-body)) de-env))]
    [(Call fun-expr arg-exprs)
     (if (= (length arg-exprs) 1)
         (CCall (preprocess fun-expr de-env)
                (preprocess (first arg-exprs) de-env))
         (preprocess
          (Call (Call fun-expr (list (first arg-exprs)))
                (rest arg-exprs))
          de-env))]
    [(WRec id named-expr bound-body)
     (sub (With id (Call Y-combinator (list (Fun (list id) named-expr)))
                bound-body))]))
    
   
(: run : String -> Number)
;; evaluate a BRANG program contained in a string
(define (run str)
  (let ([result (eval (preprocess (parse str) de-empty-env) '())])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; tests
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{with {f {with {x 3} {fun {y} {+ x y}}}}
              {with {x 100}
                {call f 4}}}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)
(test (run "{with {sub3 {fun {x} {- x 3}}}
              {call sub3 1}}")
      => -2)
(test (run "{with {mul3 {fun {x} {* x 3}}}
              {call mul3 1}}")
      => 3)
(test (run "{with {div3 {fun {x} {/ x 3}}}
              {call div3 1}}")
      => 1/3)
(test (run "{with {div3 4 {fun {x} {/ x 3}}}
              {call div3 1}}")
      =error> "bad `with' syntax in")
(test (run "{with {div3 {fun {x} }}
              {call div3 1}}")
      =error> "bad `fun' syntax in")
(test (run "{call 3 3}")
      =error> "`call' expects a function")
(test (run "{fun {a} {+ a 0}}")
      =error> "evaluation returned a non-number")
(test (run "{/ 1 {fun {a} {+ a 0}}}")
      =error> "expected a number, got")
(test (run "{call {fun {x} {? x 3}} 4}")
      =error> "bad syntax in")
(test (run "{call {fun {x} {/ x 3}} }")
      =error> "bad `call syntax in")
(test (run "{call {fun {x y z} {+ x {/ y z}}} 1 3 3}") => 2)
(define e1 (de-extend de-empty-env 'b))
(define e2 (de-extend e1 'a))
(test (e1 'a) =error> "The environment has no mapping for")
(test (e1 'b) => 0)          ; and 'b is mapped to 0
(test (e2 'a) => 0)          ; e2 maps 'a to 0
(test (e2 'b) => 1)          ; and now 'b is mapped to 1

(define minutes-spent 350)