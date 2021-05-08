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
Module "simEventClass-module"          
provides: - events and event handlers
=================================================================
|#

(provide  
  set-time-due! time-due 
  scene-protocol set-scene-protocol!
  set-event-message! eventMes
  charC mouseX mouseY mouseEvent)
 #| Auto provides:
       classes: sim-event 
       generics: display-event handle
 |#

(require
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module)

(defclass* sim-event ()
  
  (scheduled-time 
   :reader time-due
   :writer set-time-due!
   :initvalue 0
   :initarg :time-due
   :type <number>
   :allocation :instance
   :documentation
   "The scheduled time for the event to take place")
  
  (eventMessage 
   :reader eventMes
   :writer set-event-message!
   :initvalue "no event"
   :initarg :eventMes
   :allocation :class
   :type <string>
   :documentation
   "A message text describing the current event")
  
  (protocol 
   :reader scene-protocol
   :writer set-scene-protocol!
   :initvalue #t
   :initarg :scene-protocol
   :allocation :class
   :documentation 
   "When true, each event will be  traced")
  :autopred #t ; auto generate predicate sim-event?
  :printer  #t
  :documentation 
  "Simulation events to be entered in a simulation calendar"
  )

(defclass* sim-nothing-happens (sim-event)
  :autopred #t 
  :printer  #t 
  :documentation 
  "a do nothing event"
  )
; each actor in the simulation may be entered 
; as an event in the simulation calendar.

(defclass* sim-Mouse-event (sim-event)
  (x 
   :reader mouseX
   :initarg :mouseX
   :type <number>
   :documentation
   "the x coordinate of the mouse position")
  (y 
   :reader mouseY
   :initarg :mouseY
   :type <number>
   :documentation
   "the y coordinate of the mouse position")
  (event 
   :reader mouseEvent
   :initarg :mouseEvent
   :type <symbol>
   :documentation
   "(one-of button-down, button-up, drag, move, enter, leave)")
  
  :autopred #t 
  :printer  #t 
  :documentation 
  "signals mouse events")

(defclass* sim-Key-event (sim-event)
  (kCode 
   :reader charC
   :initarg :charC
   :documentation
   "the char entered")
  
  :autopred #t 
  :printer  #t 
  :documentation 
  "signals key strokes")

#|
  Warum ein Modifikator für time-due?

  Die aktiven Simulationsobjekte sollen selbst 
  als "events" fungieren können. Deshalb ist
  es notwendig, daß der Ereigniszeitpunkt variabel sein kann,
  sonst würden die Objekte kopiert werden müssen und ihre Identität
  verlieren.
|#

(defgeneric* handle ((event sim-event))
  :documentation 
  "A handler for simulation events.
   For each application specific subclass of events  
   you should provide a specific handler."
  )

(defgeneric* display-event ((event sim-event) )
  :documentation 
  "Show the event"
  :combination generic-begin-combination)
