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
         racket/trace
         se3-bib/tools-module)

(provide   
 subs interleave perms denominations count-change ?)

(require htdp/draw)
(require se3-bib/tools-module)

;;;; re-implement standard functions
(define (my-length xs)
  (if (null? xs) 0
      (+ 1 (my-length (cdr xs)))))

(define (my-list-ref xs n)
  ;;; element n of list xs, zero indexed
  (cond
    ((<= (length xs) n)
     (error "my-list-ref: Index" n "out of range for list" xs))
    ((zero? n) (car xs))
    (else (my-list-ref (cdr xs) (- n 1)))))

;;;; ======================================================================
;;;;   Kombinatorische Funktionen
;;;; ======================================================================

(define (subs xs)
  ; Die Liste aller Unterlisten von xs
  (if (null? xs) '(())
      (let ([head (car xs)]
            [tail (cdr xs)])
        (append (subs tail)
                (map (curry cons head)
                     (subs tail))))))

(define (interleave x ys)
  ; Die Liste aller Einfuegemoeglichkeiten
  ; von x in ys
  (if (null? ys) (list (list x))
      (let ([head (car ys)]
            [tail (cdr ys)])
        (append (list (cons x ys))
                (map (curry cons head)
                     (interleave x tail))))))
(define (perms xs)
  ; Die Liste aller Permutationen einer Liste
  (if (null? xs) '(())
      (apply append
             (map (curry interleave (car xs))
                  (perms (cdr xs))))))

;;; Wechsele einen Betrag in Muenzen
(define denominations 
  '((Hdollar 50) (Quarter 25) 
                 (Dime 10) (Nickel 5) (Cent 1)))

(define (count-change amount)
  (cc amount denominations)) 

(define (cc a ds)
  (cond [(= a 0) 1]
        [(null? ds) 0]
        [(< a 0) 0]
        [else 
         (let ([coin1 (cadr (car ds))]
               [other-coins (cdr ds)])
           (+ (cc (- a coin1) ds) 
              (cc  a other-coins)))]))


(define ?
  "Kombinatorische Funktionen - definierte Namen:
      (subs xs)
      (interleave x ys)
      (perms xs)
      denominations 
      (count-change amount)
      (cc a ds)
     ")