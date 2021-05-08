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
Module "simCalendarImpl-module": 
Implementation module of simCalendarClass-module
  
 provides: - a simulation calendar with a ranked queue of events
           - scheduling and dispatching of events
           - specialized events (deadlock) 
=================================================================
|#

(provide 
 *current-calendar*) ; only for testing, should better be hidden

(require
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/queuesClass-module 
  se3-bib/sim/simBase/queuesClass-module
  se3-bib/sim/simBase/queuesImpl-module
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module
  se3-bib/sim/simBase/simEventClass-module
  se3-bib/sim/simBase/simEventImpl-module
  se3-bib/sim/simBase/simCalendarClass-module)

(define *current-calendar* #f); class sim-calendar
;"The current simulation scenario, hidden,
;will be set by 'initialize'"

(defmethod 
  initialize :after 
  ((newCalendar sim-calendar) initargs)
  (setf! *current-calendar*
         newCalendar) 
  )
#|
 ;variante 2: direkt der generischen Funktion hinzufügen  
(add-method
   initialize
   (qualified-method
    :after 
    ((newCalendar sim-calendar) initargs)
    (setf! *current-calendar*
           newCalendar) 
    (if (next-method?)
        (call-next-method newCalendar initargs))))
|#

(defmethod calendar-info ((calendar sim-calendar)); 
  "request state information from the calendar."
  (writeln  "   Calendar events: ")
  (map (curryr display-event)
       (enumerate-queue calendar))
  #t)

(defmethod sim-info ((calendar sim-calendar));  
  (calendar-info calendar))

(define *dt* 0.0001); time quantum to ensure sequential events

(defmethod schedule 
  ((event sim-event) 
   time-d)
  "enter an event in the calendar"
  
  (set-time-due! 
   event 
   (max time-d (+ (now) *dt*) )) ; ensure event is scheduled as future event
  (enqueue! *current-calendar* event)
  *current-calendar*)

(defmethod peak-next-event ((calendar sim-calendar))
  (head-queue calendar))

(defmethod dispatch ((calendar sim-calendar))
  "Dispatch a calendar event to happen right now. 
      Advance the clock to the time of the event."
  (if (empty-queue? calendar)
      (schedule 
       (make sim-deadlock)
       (now))
      (let ([nextEvent (dequeue! calendar)])
        (set-clock! 
         (max (time-due nextEvent) (+ (now) *dt*) ))
        (writeln "Clock: " (now));-----
        (when (scene-protocol calendar) 
          (display-event nextEvent))
        (handle nextEvent)
        calendar)))

(defmethod handle ((event sim-deadlock))
  (print "**** Warning: Simulation deadlock occured!")
  (schedule (make sim-Quit) (now))
  #f)  


