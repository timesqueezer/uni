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
Module "simDoorClass-module"

A source of random events from a queue of sim-events:
The sim-door creates uniformly distributed events 
by scheduling the first item of the queue idle-items,
idle actors are enqueued randomly.

=================================================================
|#


#| A source of uniformly distrubuted arrival events. 
   This type of event source keeps the simulation process going 
   and has to be triggered initially to start the process.

   auto provide:
   classes: sim-door
   generics: send-backstage
           
 |#

(provide theActorInTheDoor 
         set-ActorInTheDoor!)

(require 
  se3-bib/sim/simBase/sim-base-package
  se3-bib/sim/simAppl/simCustomerClass-module)

(defclass* sim-door
  (sim-exp-event-source RANDOM-queue sim-actor-view)
  (actorInTheDoor 
   :reader theActorInTheDoor
   :writer set-ActorInTheDoor!
   :initvalue '???
   ; :type sim-actor
   :documentation "The name of the actor"
   )
  :autopred #t ; auto generate predicate queue?
  :printer  #t     
  :documentation 
  "An event source of customers picked randomly from a queue")

(defgeneric* send-backstage ((c customer))
  :documentation "make the customer c idle")

(defgeneric* arrival ((a customer))
  :documentation
  "an actor is entering the stage.")

(defgeneric* departure ((a customer))
  :documentation
  "an actor is leaving and idle again.")

(defgeneric* close-the-door ((d sim-door))
  :documentation
  "an actor is closing the door after entering the scene.")  

(defgeneric* door-empty? ((d sim-door))
  :documentation
  "is there no actor entering just now?" 
  )

#|
(require se3-bib/sim/simAppl/simDoorClass-module)
(define d (make sim-door))
|#