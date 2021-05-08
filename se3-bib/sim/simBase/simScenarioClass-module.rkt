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
Module "simScenario"
          
provides: - a controler for an event-driven discrete simulation:
          - create the event sources
          - an interface to world.ss
=================================================================
|#


(provide 
 scene-description set-scene-description!
 get-end-of-time set-end-of-time!
 max-num-events set-max-num-events!)
 #| Auto provides:
         sim-scenario 
         createTheEventSources
         simulate draw-world drawWorld 
         snapshot
 |#

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/simActorViewClass-module
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simEventClass-module
  se3-bib/sim/simBase/simCalendarClass-module)

(defclass* sim-scenario 
  (sim-clock sim-calendar sim-event sim-actor-view)
  (domain  
   :reader scene-description
   :writer set-scene-description!
   :initarg :scene-description
   :initvalue "some scenario" 
   :documentation "a string that describes the domain"
   :type <string>)   
  ;in this implementation there is only one universe
  (end-of-time
   :reader get-end-of-time
   :writer set-end-of-time!
   :initvalue 10
   :initarg :the-end-of-time
   :type <number>
   :documentation 
   "the time when the simulation will end")
  (max-num-events
   :reader max-num-events
   :writer set-max-num-events!
   :initvalue 0
   :initarg :the-max-num-events
   :type <number>
   :documentation 
   "the maximum number of events to be simulated")
  
  :documentation 
  "A simulation universe with a clock and a calendar of events"
  :autopred #t ; auto generate predicate sim-scenario?
  :printer  #t 
  :automaker #t
  )
; The sim-scenario doubles as the start-up event.
; Every application must provide a handler for this event.
; This handler must schedule an event that kicks off the simulation process
(defgeneric* simulate 
  ((the-universe sim-scenario)  &key  [tick :tick 1])
  :documentation
  "Simulate the scenario  for an interval of dt time units"
  ; tick: clock ticks in seconds, dt: simulation time in seconds
  )

(defgeneric* draw-world 
  ((universe sim-scenario) )
  :documentation
  "draw the world onto the canvas"
  )

(defgeneric* create-the-event-sources ((universe sim-scenario)) 
  :documentation 
  "create all the active scenario components")
; abstract, must be specialized by the application

(defgeneric* run ((world universe sim-scenario)
                  &key  (tick 1))
  :documentation  "a default simulation run"
  )

(defgeneric* snap-shot((universe sim-scenario))
  :documentation 
  "Show the similation time, the calendar of events,
and the state of all actors.")

(defgeneric* the-next-world ((universe sim-scenario))
  :documentation 
  "The changed world after the current event")

(defclass* sim-Snapshot (sim-event)
  :autopred #t 
  :printer  #t 
  :documentation 
  "display the state of the simulation"
  )

;For testing
#|
(define u4 
  (make 
   sim-scenario ;class of the universe
   :actor-name "The Galaxy"
   :actor-pic *default-universe-pic*
   :scene-description "Das Weltall, unendliche Weiten" 
   :the-end-of-time 10 
   :the-max-num-events 30 ))

(set-canvas-w-h! u4 
 (image-width *default-universe-pic*)
 (image-height *default-universe-pic*))
|#