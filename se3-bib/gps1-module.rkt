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

;; Das vollstaendige GPS-Programm
;; Version 1  - problematisch
;; Übersetzt nach Racket von Leonie Dreschler-Fischer, nach einer CommonLisp Vorlage von Peter Norvig

(require se3-bib/tools-module
         test-engine/scheme-tests
         (prefix-in cl: (only-in swindle some)))
#|
(define (complement pred?) 
  ; the complement of the predicate pred?
  ;defined in swindle as negate
  (compose not pred?))
|#

(define (set-difference m1 m2)
  ; the difference set of set  m1 and m2
  (filter (negate (curryr member m2)) 
          m1))
(define (set-union m1 m2)
  ; the union set of set  m1 and m2
  (append m1 (set-difference m2 m1)))

(define (find-all item sequence test?)
  ;"Find all those elements of sequence that match item,
  ; according to the test test?
  (filter (curry test? item) 
          sequence))

(define (find-all-not item sequence testnot?)  
  ;Find all those elements of sequence that do not match item,
  ;according to the test testnot?.  
  (find-all item sequence (negate testnot?)))

(struct op; An operation
        (action
         preconds 
         add-list 
         del-list))

(define(appropiate? goal op)
  ;An op is appropiate to a goal if it is in its add list.
  (member goal (op-add-list op)))

(define *school-ops*
  (list
   (op 'drive-son-to-school ; action
       '(son-at-home car-works) ;preconds
       '(son-at-school); add-list 
       '(son-at-home));del-list 
   (op 'shop-installs-battery ;action 
       '(car-needs-battery 
         shop-knows-problem shop-has-money)
       '(car-works)
       '(car-needs-battery))
   (op 'tell-shop-problem
       '(in-communication-with-shop)
       '(shop-knows-problem)
       '())
   (op 'telephone-shop
       '(know-phone-number)
       '(in-communication-with-shop)
       '())
   (op 'look-up-number
       '(have-phone-book)
       '(know-phone-number)
       '())
   (op 'give-shop-money
       '(have-money)
       '(shop-has-money)
       '(have-money))))

(define (gps theState goals op)
  ;General problem solver: achieve all goals using *ops*.
  ; theState: a list of conditions that hold
  ; ops: the operators available
  ;  (trace achieve apply-op)
  
  (define (achieve goal)
    ; A goal is achieved if it already holds,
    ; or if there is an appropriate op for it
    ; that is applicable.
    (writeln (list "trying: " goal))
    (or (member goal theState)
        (cl:some apply-op 
                 (find-all goal ops appropiate?))))
  
  
  (define (apply-op op)
    ;Print a message and update theState if op is applicable.
    (if (every achieve (op-preconds op))
        (begin 
          (writeln (list 'executing (op-action op)))
          (set! theState (set-difference theState (op-del-list op)))
          (set! theState (set-union theState (op-add-list op)))
          #t)
        #f))
  
  (if (every achieve goals)
      'solved 'not-solved))

(check-expect 
 (gps '(son-at-home        
        car-needs-battery        
        have-money        
        have-phone-book)      
      '(son-at-school)         
      *school-ops*) 'solved)

(check-expect 
 (gps '(son-at-home        
        car-needs-battery         
        have-money)     
      '(son-at-school)      
      *school-ops*) 
 'not-solved)
(check-expect 
 (gps '(son-at-home       
        car-works)      
      '(son-at-school)     
      *school-ops*) 
 'solved)
