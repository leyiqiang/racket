;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname homework1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define I-will-behave #t)

;;; This function takes three integers and returns a boolean about
;;; whether they are all at most within an interval of 2 of each other
;;; Int Int Int -> Boolean
;;; x, y, z must be integers
(define (near? x y z)
  ;; check if any of the two numbers are within an interval of 2
  (and (<= (abs (- x y)) 2)
       (<= (abs (- x z)) 2)
       (<= (abs (- y z)) 2)))

;;; tests for near?
(equal? #t (near? 1 -1 1))
(equal? #f (near? 2 -2 2))
(equal? #f (near? 0 2 3))


;;; This function takes a list of symbols and returns an integer
;;; that indicates a list of 'x's in the list
;;; (Listof symbol) -> Int
;;; symbols must be a list of symbols
(define (count-xs symbols)
  ;; if the list is empty, return 0
  (if (null? symbols) 0
      ;; else add one for each 'x met, and call count-xs recursively
      (if (eq? (first symbols) 'x)
          (+ 1 (count-xs (rest symbols)))
          (count-xs (rest symbols)))))

;;; tests for count-xs
(equal? 1 (count-xs (cons 'x (cons 'y empty))))
(equal? 0 (count-xs (cons 's (cons 'y empty))))
(equal? 2 (count-xs (cons 'x (cons 'x empty))))
(equal? 0 (count-xs empty))


;;; This function takes a list of numbers and returns a boolean about 
;;; whether they are in ascending order
;;; List of number -> Boolean
;;; numbers must be a list of numbers
(define (ascending? numbers)
  (cond
    ;; if the list is empty or the second item of the list is empty,
    ;; return true
    [(null? numbers) #t]
    [(null? (rest numbers)) #t]
    [(> (first numbers) (first (rest numbers))) false]
    [else (ascending? (rest numbers))]))

;;; tests for ascending
(equal? #t (ascending? (cons 1.3 (cons 3 (cons 5 empty)))))
(equal? #f (ascending? (cons 4 (cons 3 (cons 5 empty)))))
(equal? #t (ascending? (cons 1 (cons 3 empty))))
(equal? #t (ascending? (cons 1 (cons 1 empty))))
(equal? #t (ascending? (cons -1 (cons 0 empty))))
(equal? #t (ascending? empty))

;;; This function takes two lists of equal length,
;;; and returns a list that contains two-element lists from the first and
;;; the seconds lists respectively
;;; Listof ?, Listof ? -> List of (List ? ?)
;;; list-a and list-b must be lists and have the same length
(define (zip2 list-a list-b)
   (if (null? list-a) empty
       (cons (list (first list-a)
                   (first list-b))
             (zip2 (rest list-a)
                   (rest list-b)))))

;;; tests for zip2
(equal? (list (list 1 'a) (list 2 'b) (list 3 'c))
        (zip2 (list 1 2 3) (list 'a 'b 'c)))

(equal? empty (zip2 '() '()))

(define minutes-spent 120)