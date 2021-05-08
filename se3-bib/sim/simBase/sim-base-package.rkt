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
================================================================================
Module "sim-base-package"
          
provides: - a framework for an event-driven discrete simulation
          - a class of simulation events 
            to be extended by the applications
          - a simulation calendar to enter discrete events
          - a mixin class "simPath" for book keeping operations on waiting lines
          - a mixin class "simActor" for broadcasting to all simulation objects
================================================================================
|#

(provide 
 sim-base-test
 (all-from se3-bib/sim/simBase/sim-utility-base-package)   
 (all-from se3-bib/sim/simBase/queuesClass-module) 
 (all-from se3-bib/sim/simBase/queuesImpl-module)   
 (all-from se3-bib/sim/simBase/simActorClass-module)
 (all-from se3-bib/sim/simBase/simActorImpl-module)   
 (all-from se3-bib/sim/simBase/simClockClass-module)
 (all-from se3-bib/sim/simBase/simClockImpl-module)  
 (all-from se3-bib/sim/simBase/simEventClass-module)
 (all-from se3-bib/sim/simBase/simEventImpl-module)   
 (all-from se3-bib/sim/simBase/simEventSourceClass-module)
 (all-from se3-bib/sim/simBase/simEventSourceImpl-module)  
 (all-from se3-bib/sim/simBase/simCalendarClass-module)
 (all-from se3-bib/sim/simBase/simCalendarImpl-module)   
 (all-from se3-bib/sim/simBase/simScenarioClass-module)
 (all-from se3-bib/sim/simBase/simScenarioImpl-module)  
 (all-from se3-bib/sim/simBase/simActorViewClass-module)
 (all-from se3-bib/sim/simBase/simActorViewImpl-module)  
 (all-from se3-bib/sim/simBase/simPathClass-module)
 (all-from se3-bib/sim/simBase/simPathImpl-module))

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/queuesClass-module  
  se3-bib/sim/simBase/queuesImpl-module
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/simActorImpl-module
  se3-bib/sim/simBase/simActorViewClass-module
  se3-bib/sim/simBase/simActorViewImpl-module
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module
  se3-bib/sim/simBase/simEventClass-module
  se3-bib/sim/simBase/simEventImpl-module
  se3-bib/sim/simBase/simEventSourceClass-module
  se3-bib/sim/simBase/simEventSourceImpl-module
  se3-bib/sim/simBase/simCalendarClass-module
  se3-bib/sim/simBase/simCalendarImpl-module
  se3-bib/sim/simBase/simScenarioClass-module
  se3-bib/sim/simBase/simScenarioImpl-module
  se3-bib/sim/simBase/simPathClass-module
  se3-bib/sim/simBase/simPathImpl-module)

(define (sim-base-test)
  ; create the universe
  (make 
   sim-scenario ;class of the universe
   :actor-name "The Galaxy"
   :actor-pic *default-universe-pic*
   :scene-description "Das Weltall, unendliche Weiten" 
   :the-end-of-time 10 
   :the-max-num-events 30 )  
  (sim-info *current-universe*)
  (set-canvas-w-h! *current-universe*
                   (image-width *default-universe-pic*)
                   (image-height *default-universe-pic*))
  
  ; create an event source
  (let ([ev-s (make sim-event-source 
                    :actor-name "testEventsource")])
    ; schedule some events
    (next-normal-event ev-s)
    (next-normal-event ev-s)
    (next-normal-event ev-s)
    
    ; list the actors
    (map actor-name (the-actors *all-actors*))
    
    ; kick off the simulation loop
    (simulate *current-universe* )
    ; show the result
    (broadcast ev-s sim-info :sim-type sim-event-source)))


