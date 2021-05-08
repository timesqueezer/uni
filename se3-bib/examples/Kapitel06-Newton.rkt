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

; testcases for tools-module
; Newton Verfahren, sqrt und Ableitung

(provide deriv
         newton
         zeicheTangente tangente vieleTangenten)

(require test-engine/racket-tests
         racket/trace
         se3-bib/tools-module
         (only-in htdp/graphing graph-fun)
         (except-in 2htdp/universe space)
         2htdp/image)

(define (approx x y)
  (let ([eps 0.0001])
    (< (abs (- x y)) eps)))

(check-expect 
 (approx ((deriv sin) 0.0)
        
         (cos 0.0)) #t)
(check-expect 
 (approx ((deriv sin) 1.0) 
        
         (cos 1.0)) #t)

; Basic utilities
(check-expect ;id

 (eq? 1 (id 1)) #t)
; eval
(define (my-eval expr)
  (apply (car expr) 
         (cdr expr)))

;Newton-Verfahren
(define (msqrt x)
  (let* ([eps 0.00001]
         [good-enough? 
          (lambda (y)
            (< (abs (- (sqr y) x)) 
               eps))]
         [improve 
          (lambda (y)
            (/ (+ y (/ x y)) 
               2))])
    (iter-until 
     improve good-enough? x)))
(check-expect ;msqrt

 (exact->inexact (msqrt 4))  2.0000000929222947)

#|
;graph fun test

(graph-fun 
    (lambda (x) 
      (* (sin x)  
         (sin x)))              
    'red)
|#

(define (msqrt2 x)
  (let* ([eps 0.00001]
         [good-enough? 
          (lambda (y)
            (< (abs (- (sqr y) x)) 
               eps))]
         [improve 
          (lambda (y)
            (/ (+ y (/ x y)) 
               2))])
    (exact->inexact 
     (iter-until 
      improve good-enough? x))))

(check-expect 
 (approx (msqrt2 3)
        
                      (sqrt 3)) #t)


(define (good? x wurzelx)
  (let ([eps 0.00001])
    (< (abs (- (sqr wurzelx) x)) 
       eps)))

(define/contract analyse2
  (->d ([a number?]
        [b (and/c number? (>=/c a) (<=/c c))]
        [c number?])
       ()
       (result (lambda (r)
                 (member r
                         '(kein-Dreieck
                           gleichseitig
                           gleichschenklig
                           schiefwinklig)))))
  (lambda (a b c)
    (cond [(< (+ a b) c) 'kein-Dreieck]
          [(= a c) 'gleichseitig]
          [(= a b) 'gleichschenklig]
          [(= b c) 'gleichschenklig]
          [else    'schiefwinklig])))

(define/contract 
  msqrt3
  (->d ([x (>=/c 0)]) 
       (r (and/c  
           number? 
           (curry good? x) ))
       )
  (lambda(x)
    (let ([good-enough?
           (lambda (wurzelx) ; das Argument x an good? binden
             (good? x wurzelx))]
          [improve 
           (lambda (wurzelx)
                     (/ (+ wurzelx (/ x wurzelx)) 
                        2))])
      (iter-until 
       improve good-enough? x))))

; Kurve mit Tangenten zeichnen
(define (zeicheTangente f x farbe)
  ;zeichne die Tangente von f an der Stelle x in farbe
  ( graph-fun (tangente f x) farbe)
  )

(define (vieleTangenten f)
  ;zeichne f und mehrere Tangenten
  ( graph-fun f 'red)
  (map (curry zeicheTangente f)
       '(1 2 3 4)
       '(blue green purple cyan))
  )

 (define (vieleTangenten2 f)
  ;zeichne f und mehrere Tangenten
  (graph-fun f 'red)
  (map (curry zeicheTangente f)
       '(1 2 3 4)
       '(blue green purple cyan)) )
 
(define (viertelxqua x)
  (* 0.25  x  x))  

(define mcurry
  ;;; curry an arbitrary number of args (left to right)
  ;;; to a function of several args.
  (lambda args ;function name and args to be curried
    (let ([f (car args)]
          [curried-args (cdr args)])
      (lambda not-curried-args
        (apply f 
               (append
                 curried-args
                 not-curried-args))))))

(define mcurryr
  ;;;  curry an arbitrary number of args
  ;;;  (as the last args) to a function of several args.
  (lambda args ;function name and args to be curried
    (let ([f (car args)]
          [curried-args (cdr args)])
      (lambda not-curried-args
        (apply f 
               (append
                 not-curried-args
                 curried-args))))))


(define (n-sqrt x) ; 
  ((newton   ; solving: y*y-x = 0.
    (lambda (y) 
      (- (sqr y) x)))
   x))

(check-expect (approx (n-sqrt 4) 2) #t)
(define (cubrt x) ;   
  ((newton   ; solving y*y*y-x=0.
    (lambda (y) (- (expt y 3) x)))
   x))


(define (root x n);   
  ;;; the nth root of x
  ((newton 
    (lambda (y) (- (expt y n) x)))
   x))


(check-expect 
 (approx 
 (cubrt (expt 1.5  3))
 1.5) #t)

(vieleTangenten viertelxqua)

(display "Teste mit: ((newton sin) 0.3)")
((newton sin) 0.3)