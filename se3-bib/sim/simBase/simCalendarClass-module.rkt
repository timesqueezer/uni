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
Module "simCalendarClass-module"
          
 provides: - a simulation calendar with a ranked queue of events
           - scheduling and dispatching of events
           - specialized events (deadlock) 
=================================================================
|#

(provide sim-deadlock?
         sim-Quit?)
         #| Auto provide
              classes: sim-calendar 
              generics: dispatch schedule next-event calendar-info
          |#

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/queuesClass-module
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simEventClass-module)

(defclass* sim-calendar 
  (RANKED-queue sim-actor)
  (rank-p 
   :initvalue time-due
   :reader the-rank-p
   :documentation 
   "the rank of an event is its time to occur")
  :autopred #t ; auto generate predicate sim-calendar?
  :printer  #t 
  :documentation "The calendar of scheduled events"
  )

;;;  the simulation calendar: a ranked queue of events 

(defgeneric* peak-next-event ((calendar sim-calendar))
  :documentation 
  "The next event in the event queue")

(defgeneric* dispatch ((calendar sim-calendar))
  :documentation 
  "Dispatch a calendar event to happen right now. 
      Advance the clock to the time of the event.
     returs the calendar")

(defgeneric* schedule 
  ((event sim-event) 
   time-due )
  :documentation 
  "enter an event in the calendar"
  :combination generic-begin-combination)

(defgeneric* calendar-info ((calendar sim-calendar)); 
  :documentation
  "request state information from the calendar.")


;; special simulation events and handlers
(defclass* sim-deadlock (sim-event)
  :autopred #t ; auto generate predicate sim-deadlock?
  :printer  #t 
  :documentation 
  "The calendar has emptied out prematurely: 
    Simulation stalled"
  )

(defclass* sim-Quit (sim-event)
  :autopred #t ; auto generate predicate sim-Quit?
  :printer  #t 
  :documentation 
  "stop the simulation"
  )

;For testing:
#| 
(make sim-calendar)
(define c (make sim-calendar :actorName "newCal"))
|#