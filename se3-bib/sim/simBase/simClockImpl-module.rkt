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
Module simClockImpl-module
provides: - an Implementation of simClockClass-module
=================================================================
|#

(provide 
 now 
 set-clock! advance-clock! add-time
 *current-clock*)
 #|   
   Auto provide: sim-clock
 |#  
 
(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/simActorImpl-module
  se3-bib/sim/simBase/simClockClass-module)  

(define *current-clock* #f)
; the current simulation clock, hidden,
; will be set by initialize, whenever a new clock is created

(defmethod initialize :after 
  ((newClock sim-clock) initargs)
  (setf! *current-clock*
         newClock))

(defmethod clock-info ((clock sim-clock))
  (writeln "Clock: "  (read-sim-clock clock)))

(defmethod sim-info ((clock sim-clock))
  (clock-info clock ))

(define (set-clock! sim-time)
  "Set the clock to a specific time t."
  (set-sim-clock! *current-clock* sim-time))

(define (now) 
  "Read the simulation clock."
  (read-sim-clock *current-clock*))

(define (add-time time incr-time-units)
  "Add incr to time" 
  ; might be extended for more units of time, e.g. h m s d
  (+ time incr-time-units))

(define (advance-clock! incr-time-units)
  "Advance the simulation time by increment time units"
  (set-clock! (add-time (now) incr-time-units)))

;For testing:
#|
 (make sim-clock); a default clock 
|#  

