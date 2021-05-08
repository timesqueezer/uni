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

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; student.lisp: Chapter 7's STUDENT program to solve algebra word problems.
;;;; Split into packages by Leonie Dreschler-Fischer

(require 
 racket/trace
 test-engine/racket-tests
 se3-bib/demo-gui-module
 se3-bib/tools-module
 se3-bib/macos-module
 se3-bib/string-module
 se3-bib/pattern-matching-module
 se3-bib/translator-module
 (only-in swindle/misc  maptree)
 (prefix-in cl: (only-in swindle some)))
; die Common Lisp Variante von "some" 
; gibt das Resultat der Funktionsanwendung 
; für das erste  Element der Liste zurück, 
; das das übergebene Semiprädikat erfüllt.

(define (set-difference m1 m2)
  ; the difference set of set  m1 and m2
  (filter (negate (curryr member m2)) 
          m1))

(define (set-union m1 m2)
  ; the union set of set  m1 and m2
  (append m1 (set-difference m2 m1)))

(define (subset? m1 m2)
  ; is m1 a subset of m2?
  (null?  (set-difference m1 m2)))

(define (find-all item sequence test?)
  ;"Find all those elements of sequence that match item,
  ; according to the test test?
  (filter (curry test? item) 
          sequence))

(define (find-all-not item sequence testnot?)  
  ;Find all those elements of sequence that do not match item,
  ;according to the test testnot?.  
  (find-all item sequence (negate testnot?))
  )


(define (substit newvalue var  exp)
  (maptree 
   (lambda (x)
     (if (eqv? x var)
         newvalue ; replace the variable
         x))
   exp))

#|
(check-expect
 (substit '(+ 1 2) 'A  '(+ A B))
 '(+ (+ 1 2) B))

(check-expect 
 (substit '(+ 1 2) 'A '(+ (+ A (+ A B) A) )) 
 '(+ (+ (+ 1 2) (+ (+ 1 2) B) (+ 1 2))))
|#

;(define-struct exp (lhs op rhs)); s.o.
(define (mkexp lhs op rhs)
  (list  op lhs rhs)) ; prefix representation

(define (exp? x) (pair? x))
(define(exp-args x) 
  (cdr x))

(define (exp-lhs x) (cadr x))
(define (exp-rhs x) 
  (caddr x))
(define (exp-op x) (car x))
#|
(check-expect (mkexp 1 + 1) (list + 1 1))
(check-expect 
 (let ((aa (mkexp 1 + 2)))

   (equal? aa 
        
           (mkexp (exp-lhs aa)

                  (exp-op aa)
   
                  (exp-rhs aa))))
 #t)
|#

(define ; abbriviations for pattern variables
  *abbrivs*
  '((?x* . (?* ?x))
    (?y* . (?* ?y))))

(define (expand-abbrivs rule)
  ;expand-abbrivs: rule -> rule
  ; replace the short cuts by expanded variables
  ; as defined by *abbrivs*
  (let* ((old-pattern (rule-pattern rule))
         (response (rule-response rule))
         (new-pattern (sublis *abbrivs* old-pattern)))
    (make-rule new-pattern response)))

(define
  *student-rules* 
  (map expand-abbrivs
       '(((?x* |.|)                 ( ?x))
         ((?x* |.| ?y*)          (?x  ?y))
         ((if ?x* |,| then ?y*)  (?x ?y))
         ((if ?x* then ?y*)      (?x ?y))
         ((if ?x* |,| ?y*)       (?x ?y))
         ((?x* |,| and ?y*)      (?x ?y))
         ((find ?x* and ?y*)     
          ((= to-find-1 ?x) (= to-find-2 ?y)))
         ((find ?x*)             (= to-find ?x))
         ((?x* equals ?y*)       (= ?x ?y))
         ((?x* same as ?y*)      (= ?x ?y))
         ((?x* = ?y*)            (= ?x ?y))
         ((?x* is equal to ?y*)  (= ?x ?y))
         ((?x* is ?y*)           (= ?x ?y))
         ((?x* when you ?y*)           (= ?x ?y))
         ((?x* - ?y*)            (- ?x ?y))
         ((?x* minus ?y*)        (- ?x ?y))
         ((difference between ?x* and ?y*)  (- ?y ?x))
         ((difference ?x* and ?y*)          (- ?y ?x))
         ((?x* + ?y*)            (+ ?x ?y))
         ((?x* plus ?y*)         (+ ?x ?y))
         ((sum ?x* and ?y*)      (+ ?x ?y))
         ((product ?x* and ?y*)  (* ?x ?y))
         ((multiply ?x* by ?y*)  (* ?x ?y))
         ((divide ?x* by ?y*)  (/ ?x ?y))
         ((add ?x* to ?y*)  (+ ?x ?y))
         ((subtract ?x* from ?y*)  (- ?y ?x))
         ((?x* * ?y*)            (* ?x ?y))
         ((?x* times ?y*)        (* ?x ?y))
         ((?x* / ?y*)            (/ ?x ?y))
         ((?x* per ?y*)          (/ ?x ?y))
         ((?x* divided by ?y*)   (/ ?x ?y))
         ((half ?x*)             (/ ?x 2))
         ((one half ?x*)         (/ ?x 2))
         ((1 half ?x*)           (/ ?x 2))
         ((twice ?x*)            (* 2 ?x))
         ((square ?x*)           (* ?x ?x))
         ;; caution! don't use!
         ((?x* % less than ?y*)  (* ?y (/ (- 100 ?x) 100)))
         ((?x* % more than ?y*)  (* ?y (/ (+ 100 ?x) 100)))
         ((?x* % ?y*)            (* (/ ?x 100) ?y))
         )))
; caution: the % rules do not yet work with this simple translator
(define (noise-word? word)
  ; Is this a low-content word 
  ; which can be safely ignored?
  (member word '(a an the this number of $)))

(define *numbers*
  '((zero . 0 )
    (one . 1)
    (two .  2)
    (three . 3)
    (four . 4)
    (five . 5)
    (six . 6)
    (seven . 7)
    (eight . 8)
    (nine . 9)
    (ten . 10)))

(define *to-lower*
  '((If . if )
    (What . what)
    (Then . then)
    (The . the)
    (This . this)
    (Find .  find)
    (Same . same)
    (Equals . equals)
    (Is . is)
    (Difference . difference)
    (Minus . minus)
    (Sum . sum)
    (Product . product)
    (Half . half)))

(define (translate-numbers sentence)
  (sublis *numbers* sentence))

(define (translate-upper sentence)
  (sublis *to-lower* sentence))

(define (lexically-scan-words words)
  (translate-numbers
   (filter (negate noise-word?)
           (translate-upper words))))


(define *math-problems*
  '(
    (What do you get when you multiply six by seven ?)
    
    (Fran's age divided by Robin's height is one half Kelly's IQ |.|
         Kelly's IQ minus 80 is Robin's height |.|
         If Robin is 4 feet tall |,| how old is Fran ?)
    
    (The daily cost of living for a group is the overhead cost plus 
         the running cost for each person times the number of people in 
         the group |.|  This cost for one group equals $ 100 |,|
         and the number of people in the group is 40 |.|
         If the overhead cost is 10 times the running cost |,|
         find the overhead and running cost for each person |.|)
    
    (Fran's age divided by Robin's height is one half Kelly's IQ |.|
         Kelly's IQ minus 80 is Robin's height |.|
         If Robin is 4 feet tall |,| how old is Fran ?)))

#|
(check-expect 
 (lexically-scan-words
 (car *math-problems*)) 
 '(What do you get when you
      
        multiply 6 by 7 ?))
|#


(define (make-variable words)
  ;Create a variable name 
  ;based on the given list of words
  (first words))

(define (translate-pair pair)
  ;Translate the value part of the pair 
  ; into an equation or expression."
  (cons (binding-var pair)
        (translate-to-expression 
         (binding-val pair))))
#|
(define (replace-nested-vars expr)
  ; (substit newvalue var  exp)
  (map (lambda (newval var)
         (substit newval var expr))
       (map binding-val *glob-bindings*)
       (map binding-var *glob-bindings*)))
|#

(define *glob-bindings* '()) 
; for returning the bindings in addition to the phrase

(define (translate-to-expression words)
  "Translate an English phrase into an equation or expression."
  (or (rule-based-translator
       words  
       *student-rules*
       pat-match
       rule-pattern ; :rule-if 
       rule-response ; :rule-then
       (lambda (bindings response); :action
         (let* ((nbindings (map translate-pair bindings))
                ;(resp (replace-nested-vars nbindings response)))
                (resp (sublis nbindings response)))
           (set! *glob-bindings* nbindings)
           resp ))
       no-bindings
       )
      (make-variable words)))

(define (create-list-of-equations exp)
  ;Separate out equations embedded in nested parens."
  (cond ((null? exp)'())
        ((atom? (first exp)) (list exp))
        (else 
         (append 
          (create-list-of-equations (first exp))
          (create-list-of-equations (rest exp))))))

#|
(check-expect 
  (translate-to-expression  
    '(if z is 3 |,| what is twice z)) 
  '((= z 3) (= what (* 2 z))))

(check-expect 
  (translate-to-expression 
    (lexically-scan-words 
      '(What do you get when you multiply six by seven ?))) 
  '(= What (* 6 7)))

(check-expect  
  (translate-to-expression  
    (lexically-scan-words 
      '(Fran's age divided by Robin's height 
        is one half Kelly's IQ |.|
         Kelly's IQ minus 80 
         is Robin's height |.|
         If Robin is 4 feet tall |,|
         how old is Fran ?))) 
 '((= (/ Fran Robin) (/ Kelly 2)) 
   ((= (- Kelly 80) Robin) 
    (= If (= 4 Fran)))))

(check-expect 
  (create-list-of-equations    
    (translate-to-expression 
      (lexically-scan-words 
        '(Fran's age divided by Robin's height 
          is one half Kelly's IQ |.|
          Kelly's IQ minus 80 
          is Robin's height |.|
          If Robin is 4 feet tall |,|
          how old is Fran ?)))) 
 '((= (/ Fran Robin) (/ Kelly 2)) 
   (= (- Kelly 80) Robin) 
   (= If (= 4 Fran))))
|#

(define (binary-exp? x)
  (and (exp? x) 
       (= (length (exp-args x)) 2)))

(define (prefix->infix exp)
  ; prefix->infix: exp --> list
  ;Translate prefix to infix expressions.
  (if (atom? exp) exp
      (map prefix->infix
           (cond [(binary-exp? exp)
                  (list 
                   (exp-lhs exp) 
                   (exp-op exp) 
                   (exp-rhs exp))]
                 [else 
                  exp]))))

(define (print-equations header equations)
  ;Print a list of equations.
  (writeln header)
  (map (compose 
        writeln
        prefix->infix)
       equations) 
  #t)

;;; solving the equations

(define *operators-and-inverses*
  '((+ -) (- +) (* /) (/ *) (= =)))

(define (inverse-op op)
  (cadr (assoc op *operators-and-inverses*)))

(define (commutative? op)
  ;Is operator commutative?
  (member op '(+ * =)))

#|
(check-expect (not (commutative? '+)) #f)
(check-expect (commutative? '-) #f)
(check-expect (inverse-op '*) '/)
(check-expect (inverse-op (inverse-op '*)) '*)
|#

(define(unknown? exp)
  ; is exp an unknown to solve for?
  (symbol? exp))

(define (in-exp? x exp)
  ;True if x appears anywhere in exp
  (or (eqv? x exp)
      (and (list? exp)
           (or (in-exp? x (exp-lhs exp)) 
               (in-exp? x (exp-rhs exp))))))

(define (no-unknown? exp)
  ;Returns true if there are no unknowns in exp.
  (cond [(unknown? exp) #f]
        [(atom? exp) #t]
        [(no-unknown? (exp-lhs exp)) 
         (no-unknown? (exp-rhs exp))]
        [else #f]))

(define (one-unknown exp)
  ;Returns the single unknown in exp, 
  ;if there is exactly one.
  (cond [(unknown? exp) exp]
        [(atom? exp) #f]
        [(no-unknown? (exp-lhs exp)) 
         (one-unknown (exp-rhs exp))]
        [(no-unknown? (exp-rhs exp)) 
         (one-unknown (exp-lhs exp))]
        [else #f]))

(define (solve-arithmetic equation)
  ;Do the arithmetic for the right hand side.
  ;; This assumes that the right hand side 
  ;is in the right form.
  (mkexp (exp-lhs equation) '= 
         (eval (exp-rhs equation))))

#|
(check-expect
  (solve-arithmetic 
    (translate-to-expression 
      (lexically-scan-words 
        '(What do you get when you multiply six by seven ?)))) 
    '(= What 42))
|#

(define (isolate e x)
  "Isolate the lone x in e on the left hand side of e."
  ;; This assumes there is exactly one x in e,
  ;; and that e is an equation.
  (cond [(eq? (exp-lhs e) x)
         ;; Case I: X = A -> X = n
         e]
        [(in-exp? x (exp-rhs e))
         ;; Case II: A = f(X) -> f(X) = A
         (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x)]
        [(in-exp? x (exp-lhs (exp-lhs e)))
         ;; Case III: f(X)*A = B -> f(X) = B/A
         (isolate 
          (mkexp (exp-lhs (exp-lhs e)) '=
                 (mkexp (exp-rhs e)
                        (inverse-op (exp-op (exp-lhs e)))
                        (exp-rhs (exp-lhs e)))) x)]
        [(commutative? (exp-op (exp-lhs e)))
         ;; Case IV: A*f(X) = B -> f(X) = B/A
         (isolate 
          (mkexp (exp-rhs (exp-lhs e)) '=
                 (mkexp (exp-rhs e)
                        (inverse-op (exp-op (exp-lhs e)))
                        (exp-lhs (exp-lhs e)))) x)]
        [else ;; Case V: A/f(X) = B -> f(X) = A/B
         (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                         (mkexp (exp-lhs (exp-lhs e))
                                (exp-op (exp-lhs e))
                                (exp-rhs e))) x)]))

#|
(check-expect 
  (prefix->infix 
     (isolate '(= (/ Fran Robin) 
                  (/ Kelly 2)) 
               'Kelly)) 
     '(Kelly = ((Fran / Robin) * 2)))
     
(check-expect 
  (isolate 
    '(= If (= 4 Fran)) 
    'Fran )
 '(= Fran (= If 4)))
|#

(define (solve equations known)
  ;Solve a system of equations by constraint propagation.
  ;; Try to solve for one equation, and substitute its value into 
  ;; the others. If that doesn't work, return what is known.
  (or (cl:some 
       (lambda 
           (equation)
         (let ([x (one-unknown equation)])
           (if x
               (let ([answer 
                      (solve-arithmetic
                       (isolate equation x))])
                 (solve 
                  (substit 
                   (exp-rhs answer)
                   (exp-lhs answer)                            
                   (remove equation equations equal?))
                  (cons answer known)))
               #f))) equations)     
      known))

(define (solve-equations equations)
  ; solve-equations --> list of equations
  ; Print the equations and their solution"
  (print-equations 
   "The equations to be solved are:" 
   equations)
  (let ([theSolution 
         (solve equations '())])
    (if theSolution
        (print-equations 
         "The solution is:" theSolution)
        (writeln "no solution"))))

(define (student words)
  ;Solve certain Algebra Word Problems.
  (solve-equations 
   (create-list-of-equations
    (translate-to-expression 
     (lexically-scan-words words)))))


(define aProblem 
  '(Peter's account is six $ |.|
         Susie's account is twice of Peter's account |.|
         How much is Susie's account ?))

; Zum Testen eingeben: 
; (student aProblem)

