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
 rule-based-translator dialog-tool
 ; example
 the-opposite negation-loop)

;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; ported to racket by Leonie Dreschler-Fischer

;; caution: segment matching seems to be buggy!

(require racket/trace
         se3-bib/tools-module
         se3-bib/pattern-matching-module
         (prefix-in cl: (only-in swindle some)))
         ; die Common Lisp Variante von "some" 
         ; gibt das Resultat der Funktionsanwendung 
         ; für das erste  Element der Liste zurück, 
         ; das das übergebene Semiprädikat erfüllt.
         
#|
 mit memf satt cl:some
(define (rule-based-translator 
         input ; list 
         rules ; list of rules
         matcher; Semipredicate: 
         ;rule-if , list-of-words , list-of-bindings -> any
         rule-if ; procedure: rule -> list 
         rule-then; procedure: rule -> list
         action; procedure: list list -> list
         bindings
         )
  ;Find the first rule in rules that matches input,
  ;and apply the action to that rule.
  (let ([somerules
         (memf;lisp
          (lambda (rule)
            (matcher (rule-if rule) 
                     input
                     bindings)) rules)])
    
    (if (not (eqv? somerules fail))
        (let ([thebindings 
               (matcher (rule-if (car somerules)) 
                        input
                        bindings)])
          (action thebindings 
                  (rule-then (car somerules))))
        #f)))
|#

(define (rule-based-translator 
         input ; list 
         rules ; list of rules
         matcher; Semipredicate: 
         ;rule-if , list-of-words , list-of-bindings -> any
         rule-if ; procedure: rule -> list 
         rule-then; procedure: rule -> list
         action; procedure: list list -> list
         bindings
         )
  ;Find the first rule in rules that matches input,
  ;and apply the action to that rule.
  (cl:some;lisp
   (lambda (rule)
     (let ([result 
            (matcher (rule-if rule) 
                     input
                     bindings)])
       (if (not (eqv? result fail))
           (let ([ r (action result 
                             (rule-then rule))])
             r)
           #f)))
   rules))

(define (dialog-tool 
         get-the-input
         ; a procedure returning a list
         transformer
         ; a procedure accepting a list, returning a list
         show-the-result
         ; a procedure accepting a list
         )
  "Respond to user input using pattern matching rules."
  
  (let ((dialog 
         (lambda (x) 
           (show-the-result
            (transformer
             (get-the-input))))))
    (iter-until dialog (curry equal? '(bye)) #f) 
    'ciao!))  

;; example negate

(define *negation-rules*
  '((( no (?* ?y)) 
     ( yes ?y) )
    (( (?* ?x) do not (?* ?y))
     ( ?x do ?y )  )
    (( (?* ?x) do  (?* ?y))
     ( ?x do not ?y )  )
    (( (?* ?x) is not  (?* ?y))
     ( ?x is  ?y)  )
    (((?* ?x) is   (?* ?y))
     ( ?x is not ?y)  )
    (((?* ?x) often  (?* ?y))
     (?x seldom  ?y)  )
    (((?* ?x) never  (?* ?y))
     (?x always  ?y) )
    (((?* ?x) always  (?* ?y))
     (?x never  ?y) )
    (( (?* ?x) love  (?* ?y))
     ( ?x hate  ?y)  )
    (( (?* ?x) hate  (?* ?y))
     ( ?x love  ?y)  )
    (( (?* ?x) dont  (?* ?y))
     ( ?x do  ?y)  )
    ((bye) (bye))
    ))

(define (the-opposite words)
  ; negate a sentence using the negation rules
  ; (negate: list-of-symbols --> list-of-symbols
  (rule-based-translator 
   words; input 
   *negation-rules*; rules 
   pat-match; matcher; 
   car; rule-if 
   cadr; rule-then
   (compose flatten sublis); action
   no-bindings
   ))

(define (negation-loop)
  (display "Enter lists of words. Enter (bye) to quit")
  (dialog-tool 
   (lambda () (display "negator> ") (read))
   the-opposite
   (lambda (r) (writeln r) r)))

;Testaufruf
; (negation-loop)

