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

(provide theParentState theTransProto 
         map-search-path show-path searchpath->list
         print-path  print-Transitions
         
         ;; example: general backtracking on number states           
         bt:test-oo bt:test2-oo emptystate)

(require swindle/setf
         swindle/misc
         (all-except 
          se3-bib/tools-module
          mappend identity every some last concat))  

#|
General Backtracking
 initial-state:  an object describing the state of an
                 initial tentative solution  
 gen-states:     a generic generator function: "gen-states state"
                 creates a list of all the possible subsequent states
 is-legal?:      a generic predicate: "is-legal state" checks
                 whether a state is admissible
 is-final-state? : a generic predicate: "is-final-state state?"
                  is #t, if the state is a solution to the problem
|#

(defclass* state () ; a state of the state space search 
  (parentState      ; the parent state of this state
   ; :type state  ; buggy in compiled code
   :accessor theParentState
   :initarg :state-parent)
  (transitionProtocol ; use this slot to store any information 
   ;about the transition from the previous state to this state
   :accessor theTransProto
   :initarg :state-proto
   :type <list>
   :initvalue '()) 
  :autopred #t ; auto generate predicate state?
  :printer #t 
  :documentation    
  "the top class of states: to be specialized"
  )

(defgeneric* isStartState? ((st state)))

(defgeneric* theLevel ((st state)))

(defgeneric* equal-state? ((st1 state)(st2 state))
  :documentation 
  "are st1 and st2 the same states of the search space?
    used to check for cycles" 
  )


(defgeneric* general-backtracking-oo  
  ((initialState state))) 
(defgeneric* general-backtracking-first-solution-only-oo  
  ((initialState state)))
(defgeneric* general-backtracking-non-cyclic-oo  
  ((initialState state)))

(defgeneric* gen-states ((st state)) ; Arg. st:  a state
  :documentation "create successor states in the state space"
  )
(defgeneric* is-legal? ((st state)) ; Arg. st:  a state
  :documentation "is the state a legal state of the state space search?"
  )

(defgeneric* is-final? ((st state)) ; Arg. st:  a state
  :documentation "is the state a solution state ?"
  )

(defgeneric* show-state ((st state)) ; Arg. st:  a state
  :documentation "display the state"
  )

(defgeneric* show-solution ((st state)) ; Arg. st:  a state
  :documentation "display the solution state"
  )
(defgeneric* map-path-to-root 
  ((st state) (p <function>)))

(define emptystate (make state))

(defmethod isStartState? ((st state))
  (slot-bound? st 'parentState))

(defmethod theLevel (???) 0)

(defmethod theLevel ((st state))
  ; the distance to the root
  (if (isStartState? st)
      1
      (+ 1 (theLevel (theParentState st)))))

(defmethod show-solution ((st state)) 
  ;display the solution state
  (show-state st); default: show the state
  )

(defmethod searchpath->list ((st state))
  ; the sequence of states as list 
  ; beginning with the current states back to the root 
  (cons st
        (if (not (isStartState? st))
            ( searchpath->list (theParentState st))
            '())))

(defmethod map-path-to-root ((st state) (p <function>))
  (if (isStartState? st) (list (p st))
      (cons (p st) (map-path-to-root (theParentState st)))) )

(defmethod map-search-path (p (st state))
  ; map the searchpath using p starting at the root
  (map p (reverse (searchpath->list st))))

(defmethod show-path ((st state))
  (map-search-path show-state st))

(defmethod print-path ((st state))
  ; print the states leading to the solution
  (map-search-path print st))

(defmethod print-Transitions ((st state))
  (map print (map-search-path theTransProto st)))


(defmethod show-state  
  ((st state))
  (print st))

(defmethod general-backtracking-oo  
  ((initialState state))   
  ;; find all solutions; careful! may cause infinite loops 
  ;; result: list of solution states
  (letrec
      ((try 
        (lambda (st)
          (show-state  st)
          (cond 
            ((not (is-legal? state)) '())
            ((is-final? st) 
             (cons st ; further solutions
                   (apply append (map try (gen-states st)))))
            (else
             (apply append
                    (map try (gen-states st))))))))
    (try initialState)))

(defmethod general-backtracking-first-solution-only-oo  
  ((initialState state))
  ;; result: solution state
  ;; find the first solution, 
  ;; non deterministically using "amb"
  ;; signals an error "amb: tree exhausted", if no solution exists 
  (letrec
      ((try 
        (lambda (st)
          (amb-assert (is-legal? st)) 
          ; fail, if illegal
          (show-state  st)
          (cond 
            ((is-final? st) 
             (begin (display "Solution found: ")
                    (show-solution st)
                    st)); the solution state
            (else 
             (let ((nextState (amblist (gen-states st))))
               ; pick non-deterministically one state to try
               (let (( so (try nextState)))
                 (amb-assert so) ; try alternative if no solution is found
                 so)))))))         
    (try initialState)
    ))



; caution: Finite search spaces only!
(defmethod general-backtracking-non-cyclic-oo  
  ((initialState state))
  ; checks for loops in the search space
  ; result: list of solution states
  
  (let ((visited-states '()))
    (letrec
        ((not-visited 
          (lambda (s) 
            (null?(filter 
                   (lambda (x) (equal-state? x s))
                   visited-states))))
         (try 
          (lambda (st)
            (push! st visited-states)
            (show-state st)
            (let ((result 
                   (cond 
                     ((not (is-legal? st)) '())
                     (else
                      (let ((next-states 
                             (filter not-visited
                                     (gen-states st))))
                        (if (is-final? st) 
                            (cons st; return the solution
                                  (apply append ;further solutions
                                         (map try next-states)))
                            (apply append
                                   (map try next-states))))))))
              result))))
      (try initialState))))


;; Example
#|
test: the states are integer numbers.
      Try to reach the state  *the-num-to-reach* starting from state "1".
      gen-state produces successor states by 
      incrementing and decrementing the number of the current state.
      The backtracking terminates if the number of the state
      is outside the bounds given by "*maxdepth*".
|#

(defclass* numberstate (state) 
  ; a linear search space without loops
  (num 
   :accessor theNum
   :initarg :num
   :initvalue 1
   :documentation "a state of integer numbers")
  
  :autopred #t ; auto generate predicate numberstate?
  :printer  #f 
  :documentation    "the top class of the hierarchie of queues"
  )

#|
(defmethod initialize ((nst numberstate) initargs)
    (call-next-method); initialize the slots from the super class
    ; retrieve the initargs by keywords
    (let ((num  (my-keyword-get initargs 
                             :num (always 0))))
      (display (list " initialize: initargs: " initargs
                     " num: " num))
      (set-theNum! nst num))
    nst)
|#

(defmethod print-state  
  ((st numberstate))
  (print (theNum st)))

(defmethod show-state  
  ((st numberstate))
  (print (theNum st)))

(defclass* numberstate-cycl (numberstate) 
  ; a search space with loops
  :autopred #t ; auto generate predicate numberstate?
  :automaker #f ; auto generates constructor make-numberstate
  :printer  #t
  ;:documentation    "the top class of the hierarchie of queues"
  )

(define *maxdepth* 20) ; depth bound of the search space

(defmethod gen-states ((nst numberstate) ; Arg. st:  a state
                       )  
  ; :documentation "successor state:"
  (print nst)
  (if (< (theLevel nst) *maxdepth*)
      ;depth bound not yet reached              
      (list  (make numberstate
                   :num (+ (theNum nst) 1)
                   :state-parent nst))                      
      '() ; depth bound reached, no more successors
      ))


(defmethod gen-states ((nst numberstate-cycl) ; Arg. st:  a state
                       )  
  ; create successors num+1, num-1
  
  (if (< (theLevel nst) *maxdepth*) ;depth bound not yet reached
      (let ((theSuccessor1 
             (make numberstate 
                   :num (+ (theNum nst) 1)
                   :state-parent nst))
            (theSuccessor2 
             (make numberstate 
                   :num (- (theNum nst) 1)
                   :state-parent nst)))
        (list theSuccessor1 theSuccessor2 )
        );a list of successors of the child
      '() ; depth bound reached, no more successors
      ))

(defmethod is-legal? ((nst numberstate)) ; Arg. st:  a state
  
  ; :documentation "is the state a legal state of the state space search?"
  #t ; always legal
  )


(define *the-num-to-reach* 10); the number to be reached

(defmethod is-final? ((nst numberstate))
  ; :documentation "is the state a solution state ?" num=10
  ( equal? (abs (theNum nst)) *the-num-to-reach*))

(defmethod equal-state?
  ((nst1 numberstate) (nst2 numberstate))
  ;are the two states the same
  ( equal? (theNum nst1) (theNum nst2)))


(define (bt:test-oo)
  ; result: list of solution states
  (general-backtracking-non-cyclic-oo     
   (make numberstate-cycl )))

(define (bt:test2-oo) ; look for one solution
  ; result: one solution state
  (general-backtracking-first-solution-only-oo         
   (make numberstate )))

;For testing:
#|
(require backtracking-oo-module )
(bt:test2-oo)
;(bt:test-oo) ; infinite loop
|#

;For the missionaries problem see: missionaries-module.rkt