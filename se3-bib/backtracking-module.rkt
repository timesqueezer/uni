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

(provide general-backtracking general-backtracking2
         general-backtracking-non-cyclic
         general-backtracking-first-solution-only
         general-backtracking-first-solution-only-amb  
         bt:test  ;example: search the natural numbers
         bt:test2 ;example: search the natural numbers, search space with loops
         bt:test3 ;example: search the natural numbers, tracing
         bt:test4 ;example: search the natural numbers, using amb
         queens-b ;8-queens with general backtracking
         color-europe
         choose-color) 

(require swindle/extra ;for amb
         racket/trace
         se3-bib/tools-module 
         se3-bib/queens-module)


;;; Allgemeines Backtracking
;;;======================================================
;;; General Backtracking
;;; initial-state:  a data structure describing the state of an
;;;                 initial tentative solution  
;;; gen-states:     a generator function: "gen-states state"
;;;                 creates a list of all the possible subsequent states
;;; is-legal?:       a predicate: "is-legal state" checks
;;;                 whether a state is admissible
;;; is-final-state? : a predicate: "is-final-state state?"
;;;                 is #t, if the state is a solution to the problem
;;;=====================================================================

(define (general-backtracking  
         initial-state 
         gen-states 
         is-legal?
         is-final?)   
  ;; find all solutions; 
  ;; careful! may cause an infinite loop 
  (letrec
      ([try 
        (lambda (state)
          ; (display (list "trying: " state) )
          (cond 
            [(not (is-legal? state)) `(fail ,state)]
            [(is-final? state) 
             (cons state ; further solutions
                   (append-map 
                    try (gen-states state)))]
            [else
             (append-map 
              try (gen-states state))]))])
    (try initial-state)))

(define (general-backtracking-first-solution-only-amb  
         initialState gen-states is-legal? is-final?)
  ;; result: solution state
  ;; find the first solution, 
  ;; non deterministically using "amb"
  ;; signals an error "amb: tree exhausted", if no solution exists 
  (define (try st)
    (amb-assert (is-legal? st)) 
    ; fail, if illegal
    (cond 
      [(is-final? st) 
       (display "Solution found: ")  st]; the solution state
      [else 
       (let ([nextState 
              (amb-car (gen-states st))])
         ; pick non-deterministically one state to try
         (let ([ so (try nextState)])
           (amb-assert so) ; try alternative if no solution is found
           so))]))         
  (try initialState)
  )

(define (general-backtracking2  
         initial-state 
         gen-states 
         is-legal?
         is-final?)
  ;; find all solutions; careful! may cause infinite loop 
  ;; define instead of letrec
  (define 
    (try state)
    (display (list "trying: " state) )
    (cond 
      ((not (is-legal? state)) `(fail ,state))
      ((is-final? state) 
       (cons state ; further solutions
             (apply append 
                    (map try (gen-states state)))))
      (else
       (apply append
              (map try (gen-states state))))))
  (try initial-state))

(define (general-backtracking-first-solution-only  
         initial-state 
         gen-states 
         is-legal?
         is-final?)
  
  ;; find the first solution, may cause an infinite loop if no solution exists 
  (letrec
      ([try 
        (lambda (state)
          (cond 
            [(not (is-legal? state)) '()]
            [(is-final? state) 
             (list state) ]; the solution state
            [else
             (append-map try (gen-states state))]))])
    (try initial-state)))

; caution: Finite search spaces only!
(define (general-backtracking-non-cyclic  
         initial-state 
         gen-states 
         is-legal?
         is-final?
         show-state ; show the current state
         clear-state) ; clear the current state (if show state has side effects)
  (let ([visited-states '()])
    (letrec
        ([not-visited 
          (lambda (s) (not (member s visited-states)))]
         [try 
          (lambda (state)
            (set! visited-states (cons state visited-states))
            (show-state state)
            (let 
                ([result 
                  (cond 
                    [(not (is-legal? state)) '()]
                    [else
                     (let 
                         ([next-states 
                           (filter not-visited
                                   (gen-states state))])
                       (if (is-final? state) 
                           (cons state
                                 (apply append
                                        (map try next-states)))
                           (apply append
                                  (map try next-states))))])])
              (clear-state state)
              result))])
      (try initial-state))))

(define (general-backtrackingTrace  
           initial-state 
           gen-states 
           is-legal?
           is-final?)
    ;; find all solutions; careful! may cause infinite loop 
    ;; define instead of letrec
    (define 
      (try state)
      (display (list "trying: " state) )
      (cond 
        [(not (is-legal? state)) `(fail ,state)]
        [(is-final? state) 
         (cons state ; further solutions
               (append-map try (gen-states state)))]
        [else
         (append-map try (gen-states state))]))
      (trace try  gen-states is-legal? is-final?)
    (try initial-state))




;;; Testing

#| TEST 1:
   The states are integer numbers.
   Try to reach the state "3" starting from state "1".
   gen-state produces successor states by 
   incrementing and decrementing the number of the current state.
   The backtracking terminates if the number of the state
   is outside the bounds given by "maxit".
|#


(define (bt:test maxit)
  (general-backtracking-non-cyclic         
   1 ;initial-state 
   (lambda (state) (if (< (abs state) maxit) 
                       (list (+ state 1) 
                             (- state 1))
                       '()));  gen-states 
   (lambda (state) #t) ; is-legal?
   (compose (curry equal? 3) abs)  ;  is-final?
   (lambda (state)
     (display (list " trying: " state))); showstate
   (lambda (state) #t); clear-state do nothing
   ))

(define (bt:test2) ; look for one solution
  (general-backtracking-first-solution-only         
   1 ;initial-state 
   (lambda (state) (list (+ state 1))) ;  gen-states 
   (lambda (state) (display " trying: ")(display state)
     #t) ; is-legal?
   (curry equal? 3))) ;  is-final?

(define (bt:test3) ; trace the calls to "try"
  (general-backtrackingTrace         
   1 ;initial-state 
   (lambda (state) (list (+ state 1))) ;  gen-states 
   (lambda (state) (display " trying: ")(display state)
     (< state 10)) ; is-legal?
   (curry equal? 3))) ;  is-final?

(define (bt:test4) ; using amb
  (general-backtracking-first-solution-only-amb         
   1 ;initial-state 
   (lambda (state) (list (+ state 1))) ;  gen-states 
   (lambda (state) (display " trying: ")(display state)
     (< state 10)) ; is-legal?
   (curry equal? 3))) ;  is-final?




#| TEST 2:
   8-Queens with general backtracking
   the state is represented by a list of positions (queen-coord)
|#

(define (queens-b size)
  (let 
      ([generate-positions 
        (lambda (st)
          (let* ([next-row (add1 (length st))]
                 [columns (nats-1-n size)]
                 [next-coords 
                  (map (curry queen-coord next-row)
                       columns)])
            (map (curryr cons st) next-coords)))]
       [position-is-safe? 
        ; check the most recently added position, i.e. (car state)
        ; against the queens placed earlier, i.e. (cdr state)
        (lambda (st)
          (or (null? st)
              (safe? 
               (cdr st)  (car st))))]
       [all-rows-filled? 
        (compose (curry = size) length)])
    (general-backtracking
     '() ; initial-state , the board is empty
     generate-positions ;next states
     position-is-safe?; is-legal?
     all-rows-filled?))) ; is-final?




#| TEST 3:
   Find (non-adjacend equally) colors for the european countries
   using swindles amb-op.
|#

(define choose-color
  (lambda ()
    (amb 'red 'yellow 'blue 'white)))


(define color-europe
  (lambda ()    
    ;choose colors for each country
    (let ([p (choose-color)] ;Portugal
          [e (choose-color)] ;Spain
          [f (choose-color)] ;France
          [b (choose-color)] ;Belgium
          [h (choose-color)] ;Holland
          [g (choose-color)] ;Germany
          [l (choose-color)] ;Luxemb
          [i (choose-color)] ;Italy
          [s (choose-color)] ;Swiss
          [a (choose-color)] ;Austria
          )
      
      (struct country 
        (thename thecolor theNeighborColors) )
      ;construct the adjacency list for
      ;each country: the 1st element is
      ;the name of the country; the 2nd
      ;element is its color; the 3rd
      ;element is the list of its
      ;neighbors' colors
      (let ([portugal
             (country 'portugal p
                           (list e))]
            [spain
             (country 'spain e
                           (list f p))]
            [france
             (country 'france f
                           (list e i s b g l))]
            [belgium
             (country 'belgium b
                           (list f h l g))]
            [holland
             (country 'holland h
                           (list b g))]
            [germany
             (country 'germany g
                           (list f a s h b l))]
            [luxembourg
             (country 'luxembourg l
                           (list f b g))]
            [italy
             (country 'italy i
                           (list f a s))]
            [switzerland
             (country 'switzerland s
                           (list f i a g))]
            [austria
             (country 'austria a
                           (list i s g))])
        (let ([countries
               (list portugal spain
                     france belgium
                     holland germany
                     luxembourg
                     italy switzerland
                     austria)])
          
          ;the color of a country
          ;should not be the color of
          ;any of its neighbors
          (for-each
           (lambda (c)
             (amb-assert
              (not (memq 
                    (country-thecolor c)
                    (country-theNeighborColors c)))))
           countries)
          
          ;output the color
          ;assignment
          (for-each
           (lambda (c)
             (display (country-thename c))
             (display " ")
             (display (country-thecolor c))
             (newline))
           countries))))))