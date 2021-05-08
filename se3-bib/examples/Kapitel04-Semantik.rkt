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

(define (square x)(* x x))
 
(define (hoch4 x) (* x x x x))    

(define (konstant x y z) 1)

(define (my-if wenn dann sonst)
   (cond (wenn dann)
         (else sonst)))
