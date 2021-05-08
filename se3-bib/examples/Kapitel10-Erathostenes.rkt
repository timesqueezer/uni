#lang lazy

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(require racket/function); fuer curry und co

(define (natsAbN n)
  (cons n (natsAbN (+ n 1))))

(define (not-divisible? x y) 
  (not (= 0 (remainder x y))))

( define ( sieve stream ) 
   ( cons
     ( car stream )
     (filter
      (lambda (num) 
        ( not-divisible? num
                         (car stream))) 
      (sieve (cdr stream)))))

(define primes ( sieve (natsAbN 2) ))

(define (show-n-primes n)
  ; return a list of the first n primes
  ; force evaluation
  (!! (take n primes)))