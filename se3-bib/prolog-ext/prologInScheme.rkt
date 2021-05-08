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

;;;; some predefined prolog predicates by Leonie Dreschler-Fischer
;;;; exports all functions from the prolog library
(require 
  se3-bib/pattern-matching-module
  se3-bib/prolog/unify
  se3-bib/prolog/prologDB
  se3-bib/prolog/prolog)

(provide (all-defined-out))

; some predefined predicates

; equality
(<- (= ?x ?x)) ; do the clauses unify?
(<- (!= ?x ?y) :-
    (not = ?x ?y) ); do the clauses not unify?

; list processing
(<- (member ?item (?item . ?rest)))
(<- (member ?item (?x . ?rest)) 
    :-(member ?item ?rest))

; functional predicates, require scheme functions

; (length list length-of-x)
(<- (length () 0))
(<- (length (?x . ?rest) ?len) :- 
    (length ?rest ?lenR) 
    (is ?len (+ 1 ?lenR)))

; numbers

; (between: is x larger than a and smaller than b?
(<- (between ?a ?x ?b) :- 
    (test (< ?a ?x ?b)))
