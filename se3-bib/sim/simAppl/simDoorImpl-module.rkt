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

#|
 =================================================================
Module "simDoorImpl-module"


The sim-door creates uniformly distributed events 
by scheduling the first item of the queue idle-items,
idle actors are enqueued randomly.
 This type of event source keeps the simulation process going 
   and has to be triggered initially to start the process.
 auto provide:
   classes: sim-door
   generics: send-backstage

=================================================================
|#


(provide *the-sim-door* )

(require
  se3-bib/sim/simBase/sim-base-package
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simCustomerClass-module)

(define *the-sim-door* #f)

(defmethod initialize :after 
  ((door sim-door) initargs)
  ;(display "make-simdoor-after")
  (setf! *the-sim-door* door))
#|
  (add-method ; Variante 2, mit Tiny-Clos
    initialize
    (qualified-method
     :after 
     ((door sim-door) initargs)
     (setf! *the-sim-door*
            door) 
     (if (next-method?)
         (call-next-method door initargs))))
|#

(defmethod arrival ((a <top>))
  (display (list "unknown customer arriving." a "ignore ********." ))
  )

(defmethod arrival ((a customer))
  (let ((theMessage (string-append 
                     "Enter: " (actor-name a))))
    (set-ActorInTheDoor! *the-sim-door* a)
    (set-event-message! *the-sim-door* theMessage)))

(defmethod close-the-door ((d sim-door))
  "an actor is closing the door 
     after entering the scene."
  (set-ActorInTheDoor! d ???))

(defmethod  send-backstage ((a customer))
  "make the actor a idle"    
  ; recycle actor for the next appearance  
  (enqueue! *the-sim-door* a))

(defmethod departure  ((a customer))
  "an actor is leaving and idle again"
  (let ((theMessage (string-append  
                     "Departure: " (actor-name a))))
    (set-event-message! *the-sim-door* theMessage)
    (send-backstage a)))

(defmethod handle ((door sim-door))
  "handle a door event, schedule a customer to appear"     
  
  (unless (empty-queue? door)
    ; dispatch the next customer from the queue     
    (let ((nextCustomer (dequeue! door)))
      (display "handle door: next customer")
      (display nextCustomer)
      (arrival nextCustomer)
      (schedule nextCustomer 
                (+ (now)
                   (random-exp :1/mu 
                               (event-rate door)))))))


; schedule the next door event
;(next-exp-event door)); done by handle :after

(defmethod sim-init! ((door sim-door))
  "tell the customers that the door is ready,
     call the customers backstage"
  (broadcast door
             sim-init! 
             :sim-class customer))

(defmethod sim-start ((door sim-door))
  "schedule the first door event"
  (next-event door))

(defmethod door-empty? ((d sim-door))
  "is there no actor entering just now?"
  (not (slot-bound? d 'actorInTheDoor)))

#|
(print "loading: simDoorImpl-module")
(require 
  se3-bib/sim/simAppl/sim-application-package
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simDoorImpl-module)


(define d (make sim-door))
|#
