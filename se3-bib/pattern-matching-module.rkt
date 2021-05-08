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
  fail 
  no-bindings nb
  pat-match
  get-binding
  extend-bindings
  binding-val binding-var
  lookup
  rule-pattern
  rule-responses 
  rule-response
  make-rule
  variable?)

(require swindle/extra; fuer amb
         racket/trace
         se3-bib/tools-module)


;;; ===================================================================
;;; Pattern matching
;;; ===================================================================

(define (variable? x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbol? x) 
       (char=? (string-ref (symbol->string x) 0) #\?)))

;;; ==============================

(define fail #f); Indicates pat-match failure

(define no-bindings '((#t . #t)))
;Indicates pat-match success, with no variables.
(define nb no-bindings); abbriviation
;;; ==============================

(define (get-binding var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(define (binding-val binding)
  "Get the value part of a single binding."
  (cdr binding))

(define (binding-var binding)
  "Get the variable part of a single binding."
  (car binding))

(define (lookup var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))


;;; ==============================


(define (match-variable var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond [(not binding) 
           (extend-bindings var input bindings)]
          [(equal? input 
                   (binding-val binding)) bindings]
          [else fail])))

;;; ==============================

(define (extend-bindings var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (and (eq? bindings no-bindings))
            '()
            bindings)))

;;; ==============================

(define (pat-match pattern input bindings)
  "Match pattern against input in the context of the bindings"
  (cond [(eq? bindings fail) fail]
        [(variable? pattern)
         (match-variable pattern input bindings)]
        [(eqv? pattern input) bindings]
        [(segment-pattern? pattern)                ; ***
         (segment-match 
          pattern input bindings 0)]    ; ***
        [(and (pair? pattern) (pair? input)) 
         (pat-match (cdr pattern) 
                    (cdr input)
                    (pat-match 
                     (car pattern) (car input) 
                     bindings))]
        [else fail]))

(define (starts-with list x)
  "Is x a list whose car element is x?"
  (and (pair? list) (equal? (car list) x)))

(define (segment-pattern? pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (pair? pattern)
       (starts-with (car pattern) '?*)))

(define (segment-match pattern input bindings start)
  "Match the segment pattern ((?* var) . pat) against input."
  (let ([var (cadr (car pattern))]
        [pat (cdr pattern)])
    (if (null? pat)
        (match-variable var input bindings)
        ;; We assume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ([pos (position (car pat) input start)])
          (if (not pos)
              fail
              (let ([b2 (pat-match
                         pat (drop input pos)
                         (match-variable var 
                                         ;(take (add1 pos) input) ; bug?
                                         (take  input  pos) ; bug?
                                         bindings))])
                ;; If this match failed, try another longer one
                (if (eq? b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

;;; ==============================

(define (rule-pattern rule) (car rule))
(define (rule-responses rule) (cdr rule))
(define (rule-response rule) (cadr rule))
(define (make-rule pattern response) 
  (list pattern response))
;;; ==============================

; test: 
;(pat-match '(I need a ?X) '(I need a vacation) nb)
