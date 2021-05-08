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

;;; Mein erstes Racket-Programm: 
;;; Berechne die Hypothenuse eines Dreiecks
;;; nach dem Satz von Pythagoras
(define a 10)
(define b 20)

;; Jetzt können wir c (ueber c-Quadrat) ausrechnen
(define c 
    (sqrt (+ (* a a) 
             (* b b))))
