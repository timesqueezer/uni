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

(define (steuersatz eink)
  (cond [(< eink 10000) 0]
        [(< eink 20000) 10]
        [(< eink 40000) 30]
        [(< eink 180000) 45]
        [else 80]))

(define Tier 'Tiger) 
(case Tier
  [(Hund) 'wauwau]       
  [(Katze Tiger) 'miau]
  [(Kuh) 'muh])

(case (* 2 3)
  [(2 3 5 7) 'prime]
  [(1 4 6 8 9) 'composite]) 
(case (* 4 4)
  [(2 3 5 7) 'prime]
  [(1 4 6 8 9) 'composite]) 
(case 'm
  [(a e i o u) 'vowel]
  [(w y) 'semivowel]
  [else 'consonant])  
; Groß/kleinschreibung
(define abc 3)
(define abC 4)

(define (analyse a b c)
  (cond [(< (+ a b) c)  'kein-Dreieck]
        [(= a c)  'gleichseitig]
        [(= a b)  'gleichschenklig] 
        [(= b c)  'gleichschenklig]
        [else     'schiefwinklig]))


(define (leap? y)
 (cond  [(= 0 (remainder y 100))
         (= 0 (remainder y 400))]
          ; durch 400 teilbar
        [else 
         (= (remainder y 4) 0)]))  

