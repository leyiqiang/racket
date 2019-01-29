#lang pl 02
;; Question 1.BNF
;; ============================================================================
;; 1a.
#|
  <SE>   ::= <LIST> | <ITEM>
  <LIST> ::= (cons <SE> <LIST>)
           | (list <SE> ...)
           | (append <LIST> ...)
           | null
  <ITEM> ::= <num> | '<sym>
  |#

;; ----------------------------------------------------------------------------
;; 1b.
#| BNF for the AE language:
   The BNF is modified to the infix version
   <AE> ::= <num>
          | { <AE> + <AE> }
          | { <AE> - <AE> }
          | { <AE> * <AE> }
          | { <AE> / <AE> }
|#

;; AE abstract syntax trees
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; parses s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    ;; Code below is modified to the infix version
    [(list lhs '+ rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '- rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '* rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '/ rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> AE)
;; parses a string containing an AE expression to an AE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: eval : AE -> Number)
;; consumes an AE and computes the corresponding number
(define (eval expr)
  (cases expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (let ([r-val (eval r)])
                 (if (equal? r-val 0)
                     999
                     (/ (eval l) r-val)))]))

(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "3") => 3)
(test (run "{3 + 4}") => 7)
(test (run "{{3 - 4} + 7}") => 6)
(test (run "{+ 3 4}") =error> "bad syntax in")
(test (run "{2}") =error> "bad syntax in")
(test (run "{3 * 4}") => 12)
(test (run "{3 / 4}") => 3/4)
(test (run "{4 / 2}") => 2)
(test (run "{3 * 0}") => 0)
(test (run "{{3 / 4} * 4}") => 3)
(test (run "{3 / 0}") => 999)
(test (run "{0 / 3}") => 0)

;; ----------------------------------------------------------------------------
;; 1c.
#|
1. The problem of this MAE is that we are not sure what value the get operator
   can really obtain (i.e. It could be 2, 3 or empty). To solve this problem,
   we need to define some order for the get operator.
2.
<MAE> ::= { seq <AE> }
        | { seq { set <AE> } ... { set <AE-with-get> } ... <AE-with-get> }
        | { seq { set <AE> } ... <AE-with-get> }

<AE>  ::= <num>
        | { + <AE> <AE> }
        | { - <AE> <AE> }
        | { * <AE> <AE> }
        | { / <AE> <AE> }

<AE-with-get> ::= <num>
                | { + <AE-with-get> <AE-with-get> }
                | { - <AE-with-get> <AE-with-get> }
                | { * <AE-with-get> <AE-with-get> }
                | { / <AE-with-get> <AE-with-get> }
                | get
|#

;; ============================================================================
;; Question 2
;; This function takes a number as input
;; and produces a number which is the square of
;; the input
(: square : Number -> Number)
(define (square n) (* n n))

;; This function takes a list of numbers as input
;; and produces a number which is the sum of the
;; squares of all the numbers in the list
(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares lon)
  (foldl + 0 (map square lon)))

;; tests for square
(test (square 5) => 25)
(test (square -5) => 25)
(test (square 0) => 0)
(test (square 1/2) => 1/4)

;; tests for sum-of-squares
(test (sum-of-squares '()) => 0)
(test (sum-of-squares (list 0)) => 0)
(test (sum-of-squares (list 0 0)) => 0)
(test (sum-of-squares (list 1 3 5)) => 35)
(test (sum-of-squares (list -1 3 -5)) => 35)


;; ============================================================================
;; Question 3
;; ----------------------------------------------------------------------------
;; 3a.
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE]) 
;; ----------------------------------------------------------------------------
;; 3b.
;; This function takes a numeric function f and a binary tree,
;; it returns a binarytree of the same shape but using f for
;; values in its leaves
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map fn tree)
  (cases tree
    [(Leaf num) (Leaf (fn num))]
    [(Node lt rt) (Node (tree-map fn lt) (tree-map fn rt))]))

;; tests for 3b.
(define t1 (Leaf 0))
(define t2 (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(define t3 (Node (Node (Leaf 1) (Leaf -2)) (Node (Leaf 3) (Leaf 4))))

(test (tree-map add1 t1) => (Leaf 1))
(test (tree-map add1 t2) => (Node (Leaf 3) (Node (Leaf 4) (Leaf 5))))
(test (tree-map add1 t3) => (Node (Node (Leaf 2) (Leaf -1))
                                  (Node (Leaf 4) (Leaf 5))))

;; ----------------------------------------------------------------------------
;; 3c.
;; This function is similar to foldl but uses a function to combine
;; two results of two subtrees, if its a leaf, call
;; leaft function to convert the value of leaf into the result value
(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
(define (tree-fold combiner leafn bintree)
  (cases bintree
    [(Leaf num) (leafn num)]
    [(Node lt rt) (combiner (tree-fold combiner leafn lt)
                            (tree-fold combiner leafn rt))]))


(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order
(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

(test (tree-flatten t1) => (list 0))
(test (tree-flatten t2) => (list 2 3 4))
(test (tree-flatten t3) => (list 1 -2 3 4))

;; ----------------------------------------------------------------------------
;; 3d.
;; helper function for tree-reverse
;; it reverse the two nodes of a tree
(: reverse-node : BINTREE BINTREE -> BINTREE)
(define (reverse-node left right)
  (Node right left))
  
;; this function takes a tree and returns a reversed tree
(: tree-reverse : BINTREE -> BINTREE)
(define (tree-reverse bintree)
  (tree-fold reverse-node Leaf bintree))

(test (equal? (reverse (tree-flatten t1))
        (tree-flatten (tree-reverse t1))) => #t)
(test (equal? (reverse (tree-flatten t2))
        (tree-flatten (tree-reverse t2))) => #t)
(test (equal? (reverse (tree-flatten t3))
        (tree-flatten (tree-reverse t3))) => #t)
(test (equal? (reverse (tree-flatten t1))
        (tree-flatten (tree-reverse t3))) => #f)
(test (equal? (reverse (tree-flatten t2))
        (tree-flatten t2)) => #f)

(define minutes-spent 300)