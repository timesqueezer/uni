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

;;;; Peter Norvigs Common Lisp "prolog" 
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;;; ported to Scheme by Leonie Dreschler-Fischer
;;;; is, not, test by Leonie Dreschler-Fischer
;;;; ported to Racket by Benjamin Seppke
;;;; Thanks go to Tobias Klinke for the Bugfix w.r.t. unnamed vars

(provide
 *occurs-check*
 rename-variables
 adjoin
 unique-find-anywhere-if
 find-anywhere-if
 prove-all
 prove
 prove-not
 top-level-prove
 find-all
 count-all
 cut
 collect-prolog-vars
 show-prolog-vars
 toplevel-predic
 continue?
 variables-in
 non-anon-variable?)

(require
  racket/trace
  swindle/setf
  swindle/misc
  (all-except 
     se3-bib/tools-module
     mappend every some last concat identity) 
   se3-bib/prolog/unify
   se3-bib/prolog/prologDB
   se3-bib/pattern-matching-module)

(define (rename-variables x)
  "replace all variables in x with new ones."
  (let ((newBindings (map 
                      (lambda (var) 
                        (cons var (gensym (symbol->string var))))
                      (variables-in x))))
    (subtree newBindings x)));sublis

(define (adjoin item xs)
  (if (not (member item xs)) 
      (cons item xs)
      xs))

(define (unique-find-anywhere-if 
         predicate tree
         &optional (found-so-far '()))
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  
  (if (atom? tree)
      (if (predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
                                found-so-far))))

(define (find-anywhere-if 
         predicate tree)
  "does predicate apply to any atom in the tree?"  
  (if (atom? tree)
      (predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))


(define (prove-all goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((equal? bindings fail) fail)
        ((null? goals) bindings)
        (else (prove (car goals) bindings (cdr goals)))))

(define (prove goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-clauses (predicate goal))))
    
    (if (list? clauses)
        (some
         (lambda (clause)
           (let ((new-clause (rename-variables clause)))
             (prove-all
              (append (clause-body new-clause) other-goals)
              (unify goal (clause-head new-clause) bindings))))
         clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (clauses (cdr goal) bindings
                 other-goals))))

(define (prove-not goal bindings other-goals)
  ; a built-in "not", author L. D.-F.
  (if (prove goal bindings '())
      #f
      (prove-all other-goals bindings)))



(define *collected-goals* '())
; a collector for find-all

(define *counted-goals* 0)

(define (collect-prolog-vars toCollect bindings other-goals)
  (let ((foundResult  
         (subst-bindings bindings (car toCollect))))
    "Push each solution  onto a stack."
    (push! foundResult *collected-goals*)
    fail)); fail and try again

(define (count-prolog-vars toCollect bindings other-goals)
  (inc! *counted-goals*)
  fail); fail and try again


(define (find-all  goal bindings other-goals)
  ; find-all, built in predicate, author L. D.-F.
  ; ( findall <Term><Ziel><Liste>)
  (let ((prevCollection *collected-goals*))
    ; stack the collected goals from previous recursive calls
    (set! *collected-goals* '())   
    (let ((term (car goal))
          (thepred (cadr goal))
          (theCollection (caddr goal)))
      (prove-all 
       (list thepred
             `(collect-prolog-vars ,term, bindings, '()))
       bindings)
      ;bind the list of collected results to the variable
      (let ((newB 
             (extend-bindings 
              theCollection
              *collected-goals* 
              bindings)))
        (set! *collected-goals* prevCollection)
        ;restore the previous  collection
        (prove-all other-goals newB)))))


(define (count-all goal bindings other-goals)
  ; count, built in predicate, author L. D.-F.
  ; (count <Var> <Ziel>)
  (let ((prevCount *counted-goals*))
    ; stack the collected goals from previous recursive calls
    (set! *counted-goals* 0)   
    (let ((var (car goal))
          (thepred (cadr goal)))      
      (prove-all 
       (list thepred
             `(count-prolog-vars ,var, bindings, '()))
       bindings)
      ;bind the counted results to the variable
      (let ((newB 
             (extend-bindings 
              var
              *counted-goals* 
              bindings)))
        (set! *counted-goals* prevCount)
        ;restore the previous  collection
        (prove-all other-goals newB)))))

(define *cut-goals* '())
; a collector for all goals that have been cut successfully

(define (cut goal bindings other-goals)
  ; cut, built in predicate, author L. D.-F.
  ; ( ! <goal>)
  (if (member goal *cut-goals*) ;don't prove again
      fail
      (let ((newB (prove goal bindings '())))
        (if newB ;succesful
            (begin (push! *cut-goals* goal)
                   (prove-all other-goals newB))))))

(define (is goal bindings other-goals)
  ; Builtin predicate is: (is <var> <expr>)
  ; evaluate a functional expression <expr> 
  ; and bind the result to a variable <var>.
  ; author L. D.-F.
  (let* ((clause  
          (subst-bindings bindings goal))
         (theVar (car clause))
         (theExpr (cadr clause))
         (result 
          (racket/base-eval theExpr))
         (newB (extend-bindings theVar result bindings)))
    (prove-all other-goals newB)))

(define (test goal bindings other-goals)
  ; Builtin predicate test: (test <expr>)
  ; succeeds if the functional expression <expr> 
  ; evautes to #t
  ; author L. D.-F.
  (let* ((clause  
          (subst-bindings bindings goal))
         (theExpr (car clause))
         (result (racket/base-eval theExpr)))
    (if (not result) #f
        (prove-all other-goals bindings))))

(define (top-level-prove goals)
  (set! *cut-goals* '())
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (writeln " No.")
  (values))

(define (not-anon-gensym? var)
  (not (char-numeric? ;not generated from '?
        (string-ref (symbol->string var) 1))))

(define (show-prolog-vars vars bindings other-goals)
  "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (let ((not-anon-vars (filter not-anon-gensym? vars))) ;bugfix T. Klinke
    (if (null? not-anon-vars) 
        (writeln "Yes")
        (dolist 
         (var not-anon-vars)
         (writeln 
          var " = "
          (subst-bindings bindings var)))))
  (if (continue?)
      fail
      (prove-all other-goals bindings)))


(define (continue?)
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    ((#\;) #t)
    ((#\. ) #f)
    ((#\newline) (continue?))
    (else
     (writeln " Type ; to see more or . to stop")
     (continue?))))

(define (variables-in exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if non-anon-variable? exp))

(define (non-anon-variable? x)
  (and (variable? x) 
       (not (eqv? x '?))
       ))

(defmacro* (?- &rest goals) 
  `(top-level-prove 
    (replace-?-vars (quote ,goals))))

(store-toplevel-predicate!
 'show-prolog-vars show-prolog-vars *database*)

(store-toplevel-predicate!
 'not prove-not *database*)

(store-toplevel-predicate!
 '! cut *database*)

(store-toplevel-predicate!
 'is is *database*)

(store-toplevel-predicate!
 'test test *database*)

(store-toplevel-predicate!
 'collect-prolog-vars collect-prolog-vars *database*)

(store-toplevel-predicate!
 'count-prolog-vars count-prolog-vars *database*)

(store-toplevel-predicate!
 'findall find-all *database*)

(store-toplevel-predicate!
 'count count-all *database*)

;For tests
#|
  (trace 
    prove
    prove-all
    prove-not
   )
|#
