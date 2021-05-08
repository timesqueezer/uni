#lang swindle

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

;;;; Peter Norvigs Common Lisp "unify" 
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; ported to Scheme by Leonie Dreschler-Fischer

(provide *occurs-check*
         unify unifier
         unify-variable subst-bindings)

(require   
  swindle/setf swindle/misc
  (all-except 
   se3-bib/tools-module
   mappend  every some last concat identity)
  se3-bib/pattern-matching-module)

(define *occurs-check* #t); "Should we do the occurs check?"

(define (unify x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eqv? bindings fail) fail)
        ((equal? x y) bindings) ;eql
        ((variable? x) (unify-variable x y bindings))
        ((variable? y) (unify-variable y x bindings))
        ((and (pair? x) (pair? y)); consp
         ; (writeln "trying pair: " x y  bindings)
         (unify (cdr x) (cdr y) 
                (unify (car x) (car y) bindings)))
        (else fail)))


(define (unify-variable var x bindings)
  "Unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (unify (lookup var bindings) x bindings))
        ((and (variable? x) (get-binding x bindings))
         (unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (else (extend-bindings var x bindings))))


(define(occurs-check var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq? var x) #t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((pair? x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (#t #f)))

;;; ==============================

(define (subst-bindings bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq? bindings fail) fail)
        ((eq? bindings no-bindings) x)
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom? x) x)
        (else (cons (subst-bindings bindings (car x)); reuse-cons!!
                    (subst-bindings bindings (cdr x))))))

;;; ==============================

(define (unifier x y)
  "Return something that unifies with both x and y (or fail)."
  (subst-bindings (unify x y) x))
