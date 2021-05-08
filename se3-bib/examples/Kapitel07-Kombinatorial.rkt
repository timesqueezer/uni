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

(provide 
    subs perms interleave)

(require test-engine/racket-tests
         racket/trace
         se3-bib/tools-module)

; (length (subs xs)) = (expt 2 (length xs))
(define (subs xs)
   (if (null? xs) '(())
       (let ((head (car xs))
             (tail (cdr xs)))
       (append (subs tail)
               (map (curry cons head)
                    (subs tail))))))

(check-expect 
 (subs '(1 2 3))
 '(() (3) (2) (2 3) (1) 
 (1 3) (1 2) (1 2 3)))

(check-expect 
 (length(subs '(1 2 3))) 
 (expt 2 (length '(1 2 3)))) 

(define (interleave x ys)
  (if (null? ys) (list (list x))
      (let ((head (car ys))
            (tail (cdr ys)))
       (append (list (cons x ys))
              (map (curry cons head)
                   (interleave x tail))))))

(define (perms xs)
  (if (null? xs) '(())
   (apply append
     (map (curry interleave (car xs))
          (perms (cdr xs))))))

(define denominations '((Hdollar 50) 
  (Quarter 25) (Dime 10) (Nickel 5) (Cent 1))) 

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
