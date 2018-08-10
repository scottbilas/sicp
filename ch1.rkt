#!/data/data/com.termux/files/home/sicp/racket
#lang racket

(require sicp)
(require rackunit rackunit/text-ui)

(define ch1-tests (test-suite "ch1"

;;;
(test-case "ch1-1.1"
;;;

(check-equal? 10
  10)
(check-equal? 12
  (+ 5 3 4))
(check-eq? 8
  (- 9 1))
(check-eq? 3
  (/ 6 2))
(check-eq? 6
  (+ (* 2 4) (- 4 6)))

(define a 3)
(define b (+ a 1))

(check-eq? 19
  (+ a b (* a b)))
(check-false
  (= a b))
(check-eq? 4
  (if (and (> b a) (< b (* a b)))
      b
      a))
(check-eq? 16
  (cond
    ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25)))
(check-eq? 6
  (+ 2 (if (> b a) b a)))
(check-eq? 16
  (* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
     (+ a 1)))

) ;case

;;;
(test-case "ch1-1.2"
;;;

(check-= (/ -37 150)
  (/
    (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7))
  ) .0)

) ;case

;;;
(test-case "ch1-1.3"
;;;

(define (ss a b) (+ (* a a) (* b b)))
(define (sumg2squares a b c)
  (cond
    ((> a b) (if (> b c) (ss a b) (ss a c)))
    ((> a c) (ss a b))
    (else (ss b c))))

(check-eq? 13 (sumg2squares 1 2 3))
(check-eq? 25 (sumg2squares 4 3 2))
(check-eq? 34 (sumg2squares 3 5 1))
(check-eq? 0 (sumg2squares 0 0 0))
(check-eq? 32 (sumg2squares 4 4 3))
(check-eq? 5 (sumg2squares -3 2 1))
(check-eq? 5 (sumg2squares -3 -2 1))
(check-eq? 5 (sumg2squares -3 -2 -1))

) ;case

;;;
(test-case "ch1-1.4"
;;;

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(check-eq? 7 (a-plus-abs-b 4 3))
(check-eq? 7 (a-plus-abs-b 4 -3))

) ;case

)) ;suite;define

(run-tests ch1-tests)
