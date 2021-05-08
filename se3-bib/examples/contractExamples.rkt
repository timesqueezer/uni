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
         se3-bib/tools-module)

(define/contract 
    *x*               ; definiere Variable *x*
    (between/c 0.0 3) ; Vertrag: 0 <= *x* <= 3
  (* (random) 3))     ; Initialisierung

(define (set-*x*! x)
  (set! *x* x))

(define/contract *z*
  odd? 9)

(define/contract *zzz*
  (flat-contract odd?) -1)

(define/contract *zz*
  (flat-named-contract "Rationalzahl" rational?) 8.1)

(define contListeUngerade 
  (listof  ; Liste mit ungeraden Zahlen
  (flat-named-contract "ungerade" odd?)))

(define/contract
  *xs* contListeUngerade '(1 3 5 9 19))

(define (set*xs*! xs)
  (set! *xs* xs))
; a contract for sort

(define contListofNumber 
  (listof (flat-contract number?)))

(define contSorted<= 
  (flat-contract
   (lambda (xs)
     (apply <= xs))))

(define/contract 
  sort<
  (contListofNumber . -> . contSorted<= )
  (lambda (nums)
    (sort nums <)))

(check-expect
 ;curry
 ((curry + 1) 1)
 1)

(check-expect
 ;curry
 ((curryr / 2) 1) 
  1/2)

(check-expect
 ;curry 2 args
 ((curry min 1 2 ) 0.5)
 0.5)

(check-expect
 ;always, 3 args
 ((always) 2 3 4)
 #t)




