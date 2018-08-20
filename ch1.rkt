#!/data/data/com.termux/files/home/sicp/racket
#lang racket

(require sicp)
(require racket/trace)
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

;;;
(test-case "ch1-1.7"
;;;

(define (sqrt-iter guess last x epsilon)
  (if (good-enough? guess last epsilon)
      guess
      (sqrt-iter (improve guess x) guess x epsilon)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last epsilon)
;  (< (abs (- (* guess guess) x)) epsilon))
  (< (abs (- last guess)) (* epsilon guess)))

(define (sqrt x epsilon)
  (sqrt-iter 1.0 (+ 1.0 (* 2.0 epsilon)) x epsilon))

(check-= 3 (sqrt 9 .001) .001)
(check-= 11.705 (sqrt (+ 100 37) .001) .001)
(check-= 1.774 (sqrt (+ (sqrt 2 .001) (sqrt 3 .001)) .001) .001)

(check-= 31622.777 (sqrt 1000000000 .0001) .001)
(check-= 1000000 (sqrt 1000000000000 .0001) .001)
(check-= .01 (sqrt .0001 .000001) .00001)

) ;case

;;;
(test-case "ch1-1.8"
;;;

(define (curt-iter guess last x tolerance)
  (if (good-enough? guess last tolerance)
      guess
      (curt-iter (improve guess x) guess x tolerance)))

(define (improve guess x)
  (average
    guess
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last tolerance)
  (< (abs (- last guess)) (* tolerance guess)))

(define (curt x tolerance)
  (curt-iter 1.0 (+ 1.0 (* 2.0 tolerance)) x tolerance))

(check-= 3 (curt 27 .000001) .001)
(check-= 4.641 (curt 100 .000001) .001)
(check-= 1000 (curt 1000000000 .000001) .001)
(check-= 10000 (curt 1000000000000 .00000001) .001)
(check-= .04641 (curt .0001 .0000001) .00001)

) ;case


;; template
;;;;
;(test-case "ch1-1.8"
;;;;
;
;) ;case

)) ;suite;define

(run-tests ch1-tests)
