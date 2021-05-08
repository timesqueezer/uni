#lang racket

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(require test-engine/racket-tests
         se3-bib/fractals-module
         se3-bib/lilies-module)


(define (add x y)            
  (if (= y 0) x          ; elementarer Fall
      (add (+ x 1) (- y 1)))) ; rek. Verw.
(check-expect (add 3 4) 7)
(check-expect ;add, Arg1 = 0
 (add 0 3) 3)
; Abbilden einer Liste

(check-expect ;Rek. ohne define
 ((lambda (n)
    ((lambda (fact-iter) 
       (fact-iter fact-iter 1 1))
     (lambda (f-i product counter)
       (if (>  counter n)
           product
           (f-i f-i
                (* counter product)
                (+ counter 1))))))
   3) 6)

;; allgemeine Rekursion
(define (my-map f xs) 
  (if (null? xs) 
      '()
      (cons (f (car xs)) 
            (my-map f (cdr xs)))))

(check-expect 
 ; my-map, leere Liste

 (my-map sin '()) '())

(check-expect ; my-map, nicht-leere Liste

 (my-map sqrt '(1 4 9 16 25)) '(1 2 3 4 5))

(define (odd? x) 
  (cond ((< x 0) (odd? (abs x)))
        ((= 0 x) #f)
        (else (even? (- x 1)))))

(define (even? x) 
  (cond ((= 0 x) #t)
        (else (odd? (- x 1)))))

(check-expect ;odd

 (odd? 5) #t)
(check-expect ;odd

 (odd? 4) #f)

(define (ack x y)
  (cond ((= 0 y) 0)
        ((= 0 x) (* 2 y))
        ((= 1 y) 2)
        (else (ack (- x 1) 
                   (ack x (- y 1))))))
(check-expect ;ack
(ack 3 3) 65536)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))
(check-expect ;fib

 
 (fib 10) 55)

(define (power x n)
  ; (and (integer? n)(>= n 0) (number? x))
  ; (number? result)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))

(define (my-length xs)
  (if (null? xs) 0 
      (+ 1 
         (my-length (cdr xs)))))
(check-expect ;mylength n. leer


 (my-length '(a b c)) 3)
(check-expect ;my-length leer
 
(my-length '()) 0)


(define (length_acc xs acc)
  (if (null? xs) 
      acc
      (length_acc 
       (cdr xs) 
       (+ acc 1))))
(define (my_length_acc xs) 
  (length_acc xs 0)) 
(check-expect  (my_length_acc '(1 2 3)) 3)

(define (fib-acc n a1 a2)
  (cond ((= n 0) a1)
        ((= n 1) a2)
        (else 
         (fib-acc (- n 1) 
                  a2 
                  (+ a1 a2)))))
(define (fibE n)
  (fib-acc n 0 1))  
(check-expect (fibE 5) 5)

(define (my-reverse xs)
  (if (null? xs) '()               
      (append (my-reverse (cdr xs)) 
              (list (car xs)))))

(check-expect ;reverse m append

 (my-reverse '(M A I S)) '(S I A M))

(define (reverse-countGarbage xs len garb)
  ; zaehle garbage für my-reverse
  (if (null? xs)
      (begin     (display garb )
                 '())
      (append (reverse-countGarbage
               (cdr xs)
               (- len 1)
               (+ len garb ))
              (list (car xs)))))

(define (reverse+countG xs)
  ; zaehle garbage für my-reverse
  (reverse-countGarbage xs (length xs) 0))

(define (reverse-akk akku xs)
  (if (null? xs) akku
      (reverse-akk
       (cons (car xs) akku)
       (cdr xs))))

(define (oeko-reverse xs)
  (reverse-akk '() xs))
(define (nats n)
  ; eine Liste nit n natürlichen Zahlen
  (if (<= n 0) '()
      (cons n (nats (- n 1)))))
(check-expect ;oeko-reverse

 (oeko-reverse (nats 10)) '(1 2 3 4 5 6 7 8 9 10))

(define (timereverse n)
  (time (my-reverse (nats n)))
  #t)

(define (timeOekoreverse n)
  (time (oeko-reverse (nats n))))
#|
> (timereverse 1000)
cpu time: 26 real time: 27 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 10000)
cpu time: 8561 real time: 8640 gc time: 6565
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 20000)
cpu time: 31833 real time: 32094 gc time: 23963
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 20000)
cpu time: 32153 real time: 34961 gc time: 24070
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 30000)
cpu time: 72479 real time: 72910 gc time: 54767
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 40000)
cpu time: 131920 real time: 135541 gc time: 100370
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timereverse 50000)
cpu time: 211418 real time: 212703 gc time: 162551
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)

> (timeOekoreverse 1000)
cpu time: 0 real time: 1 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timeOekoreverse 10000)
cpu time: 6 real time: 6 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timeOekoreverse 20000)
cpu time: 9 real time: 9 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timeOekoreverse 30000)
cpu time: 12 real time: 13 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timeOekoreverse 40000)
cpu time: 18 real time: 18 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
> (timeOekoreverse 50000)
cpu time: 21 real time: 21 gc time: 0
(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 ...)
|#

(define (iters i)
  ; Linienzahl nach i Iterationen (Kochschneeflocke)
  (* 3 (expt 4 i)))

(define cpuMillesecProSeg (/ 127579 (iters 10)))
; CPUzeit pro LInie der Koch-Schneeflocke

(define (cpuIter i)
  ; Schätzen der CPU-zeit für i Iterationen (Kochschneeflocke)
  (let* ((cpuMS 
          (exact->inexact (* (iters i) cpuMillesecProSeg)))
         (secu 1000)
         (min (* secu 60))
         (std (* min 60))
         (tage (* std 24))
         (jahre (* tage 365.2425))
         (weltalter (* 15e+9 jahre)))
    (list (cons cpuMS 'ms)
          (cons (/ cpuMS secu) 'sec)
          (cons (/ cpuMS min) 'min)
          (cons (/ cpuMS std) 'std)
          (cons (/ cpuMS tage) 'tage)
          (cons (/ cpuMS jahre) 'jahre)
          (cons (/ cpuMS weltalter) 'weltalter)
          )))


(check-expect ;snowflake

 (all-snowflakes 4) 768)

(define (modRek a b)
  (cond ((< a b) a)
        ((and (<= b a)(< a (* 2 b))) 
         (- a b))
        (else (modRek (modRek a (* 2 b)) b))))
(check-expect ;modulo rekursiv

 (modRek 55 3) 1)

;; rekursive Polygone
(check-expect ;lily

 (lily2 400) #t)
(check-expect ;lily

 (lily3 400) #t)
