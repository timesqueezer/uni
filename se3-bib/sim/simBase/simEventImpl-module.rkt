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
Module "simEventImpl-module"
          
provides: - An implementation of simEventClass-module:
            events and event handlers
=================================================================
|#

(provide 
  *event-count*)
  #|  auto provide:
       classes: sim-event 
       generics: display-event handle
   |#    

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module
  se3-bib/sim/simBase/queuesClass-module
  se3-bib/sim/simBase/queuesImpl-module        
  se3-bib/sim/simBase/simEventClass-module)

(define *event-count* 0)
; "The number of events sofar"
; needs to be a global variable, 
; allocation class does not seem to work

(defmethod display-event ((event sim-event) )
  (writeln (class-of event) 
           ", time due: " (time-due event))
  event)

; generic defined in queuesClass-module
(defmethod rank ((item sim-event))
  ; a rank in a queue, item --> number
  (time-due item)
  )


; generic defined in queuesClass-module
(defmethod less? ((event-1 sim-event) (event-2 sim-event))
  "The ranking of events in a ranked queue."
  ; an earlier event has precedence before a later event
  (< (time-due event-1)
     (time-due event-2)))

(defmethod handle ((event sim-event))
  "A default handler for simulation events.
   For each application specific subclass of events  
   you should provide a specific handler."
  
  (set-event-message!
   event 
   (symbol->string (class-name (class-of event))))
  )    

(defmethod handle ((event sim-nothing-happens))
  (set-event-message! event "nothing happens event")   
  #t)

(defmethod initialize :after 
  ((event sim-event) initargs)
  (display "initialize: ")
  (display event)
  (inc! *event-count*))

