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
;; Version 2  - kein running around the block problem
;; Übersetzt nach Racket von Leonie Dreschler-Fischer,
;; nach einer CommonLisp Vorlage von Peter Norvig

(require swindle/extra ;for amb, amb-collect
         racket/trace
         se3-bib/tools-module 
         test-engine/scheme-tests
         (prefix-in cl: (only-in swindle some)))

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
  (find-all item sequence (negate testnot?)))

(define (ensure-list state)
  (if (list? state) state '()))

; testing
(define (state-check state)
  (unless (list? state)
    (error "not a list")))

(define (pretty depth what opOrGoal)
  (writeln 
   (space depth)
   what " "
   opOrGoal))

(define (protocolOp depth  what op)
  (pretty depth what 
          (op-action op)))

(define (protocolGoal depth  what goal)
  (pretty depth what goal))


; the representation of operations
(struct op; An operation
        (action
         preconds 
         add-list 
         del-list) )

(define (print-op theOp)
  (writeln "Action: " (op-action theOp))
  (writeln "Preconds: " (op-preconds  theOp))
  (writeln "Add list: " (op-add-list  theOp))
  (writeln "Del-list: " (op-del-list theOp)))

(define (make-ex-op 
         action 
         preconds 
         add-list 
         del-list)
  ;Make a new constructor that obeys the (EXECUTING op) convention.
  ; the add-list starts with (EXECUTING op) .
  (op  
   action  
   preconds 
   (cons (list 'executing action)
         add-list)
   del-list))

;;; Operations

(define (starts-with? xs x)
  ;Is this a list whose first element is x?
  (and (pair? xs) (eqv? (car xs) x)))

(define (executing? x)
  ; Is x of the form: (executing ...)?
  (starts-with? x 'executing))

(define (action? x)
  ;Is x something that is (start) or (executing ...)?
  (or (equal? x '(start)) (executing? x)))

(define (appropiate? goal op)
  ;An op is appropiate to a goal if it is in its add list.
  (member goal (op-add-list op)))


(define (apply-op op state goal goal-stack ops)
  ;Return a new, transformed state if op is applicable.
  (state-check state)
  
  (protocolOp (length goal-stack) "Consider: " op)
  
  (let ([state2 (ensure-list
                 (achieve-all
                 state 
                 (op-preconds op)
                 (cons goal goal-stack) ops))])
    (if  (not (null? state2))
        ;; Return an updated state
        (begin
          (protocolOp (length goal-stack) 
                      "Action: " op)
          
          (set-difference
           (set-union state2 
                      (op-add-list op))
           (op-del-list op)))        
        '())))

(define (achieve state goal goal-stack ops)
  ;A goal is achieved if it already holds,
  ; or if there is an appropiate op for it that is applicable.
  ;    (state-check state)
;  (display "Goal state: ")(display state)
  (protocolGoal (length goal-stack) 
                " Goal: " goal)
  
  (cond [(member goal (ensure-list state)) state]
        [(member goal goal-stack) '()]
        [else (cl:some 
               (curryr apply-op 
                       state goal goal-stack ops)
               (find-all goal ops appropiate?))])
  )

(define (achieve-all 
         state goals goal-stack ops)
  (state-check state)
  ;Achieve each goal, and make sure 
  ;they still hold at the end.
  (let ((current-state state))
    (if (and (every
              (lambda (g)
                (set! current-state
                      (achieve 
                       current-state 
                       g 
                       goal-stack ops))
                (not (null? current-state)))
              goals)
             (list? current-state)
             (subset? goals current-state))
        current-state '()
        )))

(define (gps2 state goals ops)
  ;General problem solver: achieve all goals using *ops*.
  ; *state*: a list of conditions that hold
  ; ops: the operators available
  ;  (trace achieve apply-op)
  (state-check state)
  (filter action?
          (achieve-all 
           (cons '(start) state) 
           goals '() ops)))

;;; Operations

(define *school-ops*
  (list
   (make-ex-op 'drive-son-to-school ; action
               '(son-at-home car-works) ;preconds
               '(son-at-school); add-list 
               '(son-at-home));del-list 
   (make-ex-op 'shop-installs-battery  
               '(car-needs-battery 
                 shop-knows-problem shop-has-money)
               '(car-works)
               '(car-needs-battery))
   (make-ex-op 'tell-shop-problem
               '(in-communication-with-shop)
               '(shop-knows-problem)
               '())
   (make-ex-op 'telephone-shop
               '(know-phone-number)
               '(in-communication-with-shop)
               '())
   (make-ex-op 'look-up-number
               '(have-phone-book)
               '(know-phone-number)
               '())
   (make-ex-op 'give-shop-money
               '(have-money)
               '(shop-has-money)
               '(have-money))))


(define *banana-ops*
  (list 
   (make-ex-op 'climb-on-chair
               '(chair-at-middle-room 
                 at-middle-room on-floor)
               '(at-bananas on-chair)
               '(at-middle-room on-floor))
   (make-ex-op 'push-chair-from-door-to-middle-room
               '(chair-at-door at-door)
               '(chair-at-middle-room at-middle-room)
               '(chair-at-door at-door))
   (make-ex-op 'walk-from-door-to-middle-room
               '(at-door on-floor)
               '(at-middle-room)
               '(at-door))
   (make-ex-op 'grasp-bananas
               '(at-bananas empty-handed)
               '(has-bananas)
               '(empty-handed))
   (make-ex-op 'drop-ball
               '(has-ball)
               '(empty-handed)
               '(has-ball))
   (make-ex-op 'eat-bananas
               '(has-bananas)
               '(empty-handed not-hungry)
               '(has-bananas hungry))))

;;;; A hole in the bucket, Ein Loch ist im Eimer
;;;;==========================================================
(define *eimer-ops*
  (list
   (make-ex-op  'Loch-mit-Stroh-verstopfen
                '(Habe-Eimer Eimer-hat-ein-Loch Habe-Stroh Stroh-ist-kurz)
                '(Eimer-ist-heil)
                '(Eimer-hat-ein-Loch Habe-Stroh))
   (make-ex-op  'Stroh-abhacken
                '(Habe-Axt Habe-Stroh Axt-ist-scharf Stroh-ist-zu-lang)
                '(Stroh-ist-kurz)
                '(Stroh-ist-zu-lang))
   (make-ex-op  'Schaerfe-Axt
                '(Axt-ist-stumpf Habe-Axt Habe-Stein Stein-ist-nass)
                '(Axt-ist-scharf)
                '(Axt-ist-stumpf))
   (make-ex-op  'Benetze-Stein
                '(Habe-Stein Habe-Wasser Stein-ist-trocken)
                '(Stein-ist-nass)
                '(Stein-ist-trocken))
   (make-ex-op 'Hole-Wasser
               '(Habe-Eimer Eimer-ist-heil)
               '(Habe-Wasser)
               '())
   ))


(check-expect  
 (gps2 
  '(son-at-home
    car-needs-battery 
    have-money 
    have-phone-book)       
  '(son-at-school) 
  *school-ops*) 
 '((start)
   (executing look-up-number)
   (executing telephone-shop)
   (executing tell-shop-problem)
   (executing give-shop-money)
   (executing shop-installs-battery)
   (executing drive-son-to-school)))

(check-expect  
 (gps2 
  '(at-door         
    on-floor       
    has-ball       
    hungry     
    chair-at-door)  
  '(not-hungry)     
  *banana-ops*) 
 '((start) 
   (executing push-chair-from-door-to-middle-room)
   (executing climb-on-chair)
   (executing drop-ball) 
   (executing grasp-bananas)   
   (executing eat-bananas)))

(check-expect 
 (gps2 
  '(Habe-Eimer   
    Eimer-hat-ein-Loch
    Habe-Stroh  
    Stroh-ist-zu-lang
    Habe-Stein
    Stein-ist-trocken     
    Habe-Axt 
    Axt-ist-stumpf
    Habe-Wasser)
  '(Eimer-ist-heil)
  *eimer-ops*) 
 '((start)))

(check-expect 
 (gps2 
  '(Habe-Eimer 
    Eimer-hat-ein-Loch            
    Habe-Stroh
    Stroh-ist-zu-lang
    Habe-Stein 
    Stein-ist-trocken 
    Habe-Axt
    Axt-ist-stumpf)
  '(Eimer-ist-heil) 
  *eimer-ops*) 
 '())




;;; Mazes

(define (make-maze-op here there)
  ;Make an operator to move between two places
  ; make-maze-op : integer integer --> op
  (make-ex-op 
   `(move from ,here to ,there); action 
   `((at ,here));preconds 
   `((at ,there));add-list 
   `((at ,here))); del-list
  )
(define (make-maze-ops connection)
  ;Make maze ops in both directions
  ;make-maze-ops: pair of numbers -> op
  (list (make-maze-op (first connection) 
                      (second connection))
        (make-maze-op (second connection)
                      (first connection))))

(define *maze-ops*
  (append-map;mappend
   make-maze-ops 
   '(( 1  2)( 2  3)( 3  4)( 4  9)( 9 14)( 9  8)
     ( 8  7)( 7 12)(12 13)(12 11)(11  6)(11 16)
     (16 17)(17 22)(21 22)(22 23)(23 18)(23 24)
     (24 19)(19 20)(20 15)(15 10)(10  5)(20 25))))

(define (getDest executing)
  ;Find the Y in
  ;(executing (move from X to Y))
  
  (if (eqv? (car executing) 'executing)
      (last (second executing))
      executing))

(define (find-path start end)
  ;Search a maze for a path from start to end."
  (let ((results 
         (gps2 `((at ,start)) `((at ,end)) *maze-ops*)))
    (unless (null? results)
      (cons start (cdr
                   ;remove start marker, ad start number
                   (map getDest results))))))

(check-expect 
 (find-path 16 25) 
 '(16 17 22 23 24 19 20 25))

;;; the blocks world

(define (move-ons a b c)
  ; all add-ons for moving 'a from 'b to 'c
  (if (eqv?  b 'table)
      `(( ,a on ,c))
      `((,a on ,c)
        (space on ,b))))

(define (move-op a b c)
  ;Make an operator to move A from B to c."
  (make-ex-op 
   `(move ,a from ,b to ,c); action
   `((space on ,a) 
     (space on ,c)
     (,a on ,b)) ; :preconds 
   (move-ons a b c); :add-list 
   (move-ons a c b))); :del-list 

(define (make-block-ops blocks)
  (flatten
   (amb-collect
    (let ((a (amb-car blocks))
          (b (amb-car blocks)))
      ; you can't move a block onto itself
      (amb-assert (not (eqv? a b)))
      (let* ([c (amb-car blocks)]
             [from-to-table 
              (list  (move-op a 'table b)
                     (move-op a b 'table))]
             [from-block-to-block
              (move-op a b c)])
        (if (or (eqv? c a) (eqv? c b))
            from-to-table
            (cons from-block-to-block from-to-table)))))))

(check-expect 
 (gps2 '((a on table)         
         (b on table)        
         (space on a)         
         (space on b)         
         (space on table))       
       '((a on b)          
         (b on table))      
       (make-block-ops '(a b))) 
 '((start) 
   (executing (move a from table to b))))

(define blocks '(red-block green-block blue-block))
(define *block-ops* (make-block-ops '(a b c)))

#|
;klappt nicht, warum?
(gps2 '((a on b) (b on c) (c on table)
                 (space on a)(space on table))
      '((a on table) (b on a) (c on b))
      (make-block-ops '( a b c)))
|#