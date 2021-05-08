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

(provide 
 missionaries-puzzle mdemo
 missionary time-of 
 m-state m-state-left m-state-right
 make-initial-missio-state
 all-pairs remove-from-list  ; list utilities
 theTransProto
 print-Transitions showGroup
 mdemo)

(require swindle/misc
         swindle/setf
         (all-except 
           se3-bib/tools-module
           mappend identity every some last concat)
         se3-bib/backtracking-oo-module)

#|
    Backtracking example: Missionaries and torch
    Each missionary is represented by an integer number
    stating the time he requires to cross the bridge.
 |#



; bug-fix: keyword-get in "kw.ss" does not seem to be reliable
(define (my-keyword-get initargs theKey notfoundThunk)
  ; initargs are represented 
  ; as a list of alternating keys and values
  (let ((keylist (member theKey initargs)))
    (if (and keylist (> (length keylist) 1)) 
        (cadr keylist)
        (notfoundThunk)))
  ); end keyword-get

(defclass m-state (state)
  ; a state of the backtracking search
  (ms-left     
   :accessor left-group 
   ; a list of missionaries waiting on the left side of the bridge
   :type <list>
   :initarg :leftG
   :inivalue '()
   )
  (ms-right ; the missionaries that have reached the right side of the bridge
   :accessor right-group
   :initarg :rightG
   :type <list>
   :inivalue '()
   )
  (timeLeft ; how many minutes are left, till the battery is empty?
   :initarg :timeL
   :accessor time-left 
   :type <number>
   )
  :autopred #t ; auto generate predicate missionary?
  ;  :automaker #t ; auto generates constructor make-m-state
  :printer  #t 
  )

(defgeneric torchpos ((st m-state)); abstract!
  ;returns the position of the torch, either 'left or 'right
  )

(defclass m-state-left (m-state)
  ; a state of the backtracking search, the torch is left of the bridge
  :autopred #t ; auto generate predicate missionary?
  :printer  #t 
  )

(defclass m-state-right (m-state)
  ; a state of the backtracking search, the torch is left of the bridge
  :autopred #t ; auto generate predicate missionary?
  :printer  #t 
  )

(defmethod torchpos ((mst m-state-left)) 
  'left ; the torch is at the left side of the bridge
  )
(defmethod torchpos ((mst m-state-right)); 
  'right  ; the torch is at the right side of the bridge
  )

(defclass missionary ()
  (time-to-cross     
   :accessor time-of;  the time it takes to cross the bridge
   :initarg :ttc
   )   
  :autopred #t ; auto generate predicate missionary?
  :printer  #t 
  )

(defmethod initialize ((m missionary) initargs)
  (call-next-method); initialize the slots from the super class
  ; retrieve the initargs by keywords    
  (set-time-of! 
   m
   (my-keyword-get initargs :ttc (always 5)))
  m )

(define (make-initial-missio-state
         listOfTimes
         timeLimit)
  (make m-state-left 
        ; all missionaries are still on the 
        ; left side of the swinging bridge
        :state-parent ??? ; undefined
        :leftG (map (lambda (tim)
                      (make missionary :ttc tim))
                    listOfTimes)
        :rightG '()
        :timeL timeLimit
        :state-proto '(start)))

(define (all-pairs xs)
  ; generate a list of all selections of two elements of xs
  ; xs must not contain duplicate elements
  (if (null? xs) '()
      (append 
       (map (lambda (a) (cons (car xs) a)) (cdr xs))
       (all-pairs (cdr xs)))))

(define (remove-from-list m l)
  (filter (lambda (mm) 
            (not (eq? mm m)))
          l) )

; Strategy: only one missionary walks back to the left side
(defmethod move->left ((mst m-state-right) (m missionary))
  ; create a new state with the missionary m 
  ; moved to the left side of the swinging bridge
  (let ((newGroupRight (remove-from-list m (right-group mst)))
        (newGroupLeft (cons m (left-group mst)))
        (newTimeLeft (- (time-left mst) (time-of m)))
        (theProt (list (time-of m) '-> 'left)))
    
    (make m-state-left 
          ; all missionaries are still on the 
          ; left side of the swinging bridge
          :state-parent mst
          :leftG newGroupLeft
          :rightG newGroupRight
          :timeL newTimeLeft
          :state-proto theProt )     
    ))
; Strategy: two missionaries walks to the right side

(defmethod move->right ((mst m-state-left)
                        (m1 missionary)
                        (m2 missionary))
  ; create a new state with the missionary m1 and m2 
  ; moved to the right side of the swinging bridge
  (let ((newGroupLeft 
         (remove-from-list m1 
                           (remove-from-list m2 
                                             (left-group mst))))
        (newGroupRight (cons m1 (cons m2 (right-group mst))))
        (newTimeLeft (- (time-left mst)
                        (max (time-of m1) (time-of m2))))
        (theProt (list (list (time-of m1)
                             (time-of m2)) '-> 'right)))
    
    (make m-state-right
          ; all missionaries are still on the 
          ; left side of the swinging bridge
          :state-parent mst
          :leftG newGroupLeft
          :rightG newGroupRight
          :timeL newTimeLeft
          :state-proto theProt )     
    ))

(defmethod  gen-states ((mst m-state-left)) 
  ; a list of all successor states with two missionaries
  ; crossing the swinging bridge from left to right
  
  (map (lambda (pair)
         (let ((m1 (car pair))
               (m2 (cdr pair)))
           ( move->right mst m1 m2)))
       (all-pairs (left-group mst))))

(defmethod  gen-states ((mst m-state-right)) 
  ; a list of all successor states with one missionary
  ; walking back to the left side of the swinging bridge
  (map (lambda (a) (move->left mst a))
       (right-group mst)))

(defmethod is-legal? ((mst m-state)) 
  (>= (time-left mst) 0))

(defmethod is-final? ((mst m-state)) 
  (null? (left-group mst)))

(defmethod showGroup ((ms))
  (map time-of ms))

(defmethod show-state ((mst m-state))
  (writeln "left: " (showGroup (left-group mst))
           " right: " (showGroup (right-group mst))
           " torch: " (torchpos mst )
           " timeleft: " (time-left mst)))

(defmethod show-solution ((mst m-state))
  (writeln)
  (writeln "length of path: " 
           (length (map-path-to-root  mst show-state))))

(define (missionaries-puzzle 
         mtimes ; a list of missionary crossing times
         torchtime ; number: how long will the battery 
         ;of the torch last?
         )
  (general-backtracking-first-solution-only-oo 
   (make-initial-missio-state
    mtimes
    torchtime)))

(define (mdemo) 
  (show-solution 
   (missionaries-puzzle '(5 10 20 25) 60))
  #t)

;For testing
#|
 (require se3-bib/missionaries-module)
 (mdemo)
|#