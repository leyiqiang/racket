;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname homework1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define I-will-behave #t)

;;; This function takes three integers and returns a boolean about
;;; whether they are all at most within an interval of 2 of each other
;;; Int Int Int -> Boolean
(define (near? x y z)
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
(define (count-xs symbols)
  (if (null? symbols) 0
      (if (eq? (first symbols) 'x)
          (+ 1 (count-xs (rest symbols)))
          (count-xs (rest symbols)))))

;;; tests for count-xs
(equal? 1 (count-xs (cons 'x (cons 'y empty))))
