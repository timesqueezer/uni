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
Module "simScenarioImpl-module"
          
provides: the controller for event driven simulation: 
          the interface to htdp2/universe
=================================================================
|#

(provide 
 verbose silent stepping unstep *stepping*
 *current-universe* *default-tick*)
 #| Auto provides:
      sim-scenario 
      simulate
      draw-world
  |#

(require
 se3-bib/sim/simBase/sim-utility-base-package
 se3-bib/sim/simBase/simActorClass-module
 se3-bib/sim/simBase/simActorViewClass-module
 se3-bib/sim/simBase/simActorViewImpl-module
 se3-bib/sim/simBase/simActorImpl-module
 se3-bib/sim/simBase/simClockClass-module
 se3-bib/sim/simBase/simClockImpl-module
 se3-bib/sim/simBase/simEventClass-module
 se3-bib/sim/simBase/simEventSourceClass-module
 se3-bib/sim/simBase/simEventImpl-module    
 se3-bib/sim/simBase/simCalendarClass-module
 se3-bib/sim/simBase/simCalendarImpl-module   
 se3-bib/sim/simBase/simScenarioClass-module)

(define *current-universe* #f)
(define *default-tick* 0.5); 0.5 seconds between clock ticks

#|
; the constructor, Variante 2
  (add-method
   initialize
   (qualified-method
    :after 
    ((universe sim-scenario) initargs)
    (setf! *current-universe*
           universe) 
    (if (next-method?)
        (call-next-method universe initargs))))
|#

; the constructor
(defmethod initialize :after 
  ((the-universe sim-scenario) initargs)
  (setf! *current-universe* the-universe)
  )

;;; ============================================================
;;; Inspection, protocol
;;; ============================================================

(defmethod snap-shot((the-universe sim-scenario))
  "Show the similation time, the calendar of events,
and the state of all actors."
  (let ([oldProto (scene-protocol the-universe)])
    (set-scene-protocol! the-universe #t)
    (broadcast the-universe sim-info)  
    (set-scene-protocol! the-universe oldProto)      
    #t)) 

;;; ==========================================================

(defmethod sim-init!  ((the-universe sim-scenario));progn
  "Reset the scenario for a new simulation run"   
  (broadcast the-universe 
             sim-init! 
             :sim-class sim-event-source)
  
  #t )

(defmethod last-world? ((the-universe sim-scenario))
  (if (or 
       (sim-Quit? 
        (peak-next-event the-universe))
       (>= (now)
           (get-end-of-time the-universe))
       (>= *event-count* 
           (max-num-events the-universe)))
      #t #f))



(defmethod sim-info ((the-universe sim-scenario)) 
  ;   (clock-info universe)
  ;   (calendar-info universe)
  (writeln "Simulation scenario: " 
           (scene-description the-universe)) ; (scene-description universe))
  #t )

(defmethod the-next-world ((the-universe sim-scenario)) 
  ;    (writeln "Clock: " (now))
  (when  *stepping* 
    (writeln "break: return for continue" )(read))
  (dispatch the-universe)
  the-universe)

(define *stepping* #f)

(define (stepping) (set! *stepping* #t))

(define (unstep) (set! *stepping* #f))

(define (handle-key world key) 
  (schedule 
   (make sim-Key-event :charC key) 
   (now))   
  world)

(define (handle-mouse world x y event)   
  (display event)
  (schedule (make sim-Mouse-event
                  :mouseX x 
                  :mouseY y 
                  :mouseEvent event)
            (now) )     
  world)

(defmethod handle ((event sim-Quit))
  (set-event-message!  event "end of time, game over")
  (display "end of time, game over")
  )

(defmethod handle ((event sim-Snapshot))
  (set-event-message!  event "cheese: snapshot") 
  (snap-shot *current-universe*))

(defmethod handle ((event sim-Mouse-event)) 
  (writeln 
   "MouseX: " (mouseX event)
   ",MouseY: " (mouseY event) 
   ", Event: " (mouseEvent event))
  )

(defmethod handle ((event sim-Key-event)) 
  (writeln (actor-name *current-universe*)
           ": handle key event")
  (case (charC event)
    [(#\s) (snap-shot *current-universe*)]
    [(#\q) (writeln (actor-name *current-universe*)
                    ": received quit!")
           (schedule (make sim-Quit) 0)]
    [(#\t) (writeln (actor-name *current-universe*)
                    ": received silent!")(silent)]
    [(#\v) (verbose)]
    [else (display ".")]
    ) )

(define (verbose)
  "Print out each event"
  (set-scene-protocol! *current-universe* #t))

(define (silent) 
  "Turn off printing of events"
  (set-scene-protocol! *current-universe* #f))

(defmethod handle ((event sim-scenario))
  "activate the other even sources"
  (broadcast *current-universe* 
             sim-start 
             :sim-class sim-event-source)
  ; schedule the start up events
  )

(defmethod create-the-event-sources 
  ((universe sim-scenario)) ;abstract
  #t)

(defmethod create-the-event-sources :after
  ((the-universe sim-scenario)) 
  (display "\n create-the-event-sources :after\n")
  (sim-init! the-universe)
  (schedule (make sim-nothing-happens) (now))
  (schedule the-universe (now))
  (schedule (make sim-Quit) 
            (get-end-of-time the-universe))
  #t)

(defmethod draw-world 
  ((the-universe sim-scenario) )  
  ;draw the world onto the canvas, return a scene object
  (sim-info the-universe)
  ;(put-pinhole (actor-picture universe) 0 0)
  (place-image/align	
   (actor-picture the-universe)	 	 
   0; x	 	 	 	 
   0; y	 	 	 	 
   "left"; x-place	 	 	 	 
   "top"; y-place	 	 	 	 
   (bg-screen the-universe) ; from simActorView
   )  
  )

(defmethod simulate 
  ((the-universe sim-scenario)  
   &key  [tick :tick *default-tick*] )
  ; Simulate the scenario until (get-end-of-time the-universe)
  ; or until max-num-events have occurred
  ; tick: clock ticks in seconds (real time), 
  
  
  ;Trigger the start-up actions, get the system going    
  (big-bang 
   the-universe
   (to-draw draw-world
            (canvas-width the-universe) 
            (canvas-height the-universe))
   (on-tick  the-next-world tick)
   (on-key handle-key)
   (stop-when last-world?)
   ;(on-mouse handle-mouse) ; todo: buggy, why?
   )
  the-universe
  )

  (defmethod run ((world sim-scenario)
                  &key (tick *default-tick*))
    "a default simulation run"
    (print "\n start Simulation run \n \n ***\n")
    ; set up the actors of the universe
    (create-the-event-sources world)
    ; kick off the simulation run
    (simulate world :tick tick  ))

;For testing
#|

(require 
 se3-bib/sim/simBase/sim-utility-base-package
 se3-bib/sim/simBase/simActorViewClass-module)

(define u4 
  (make 
   sim-scenario ;class of the universe
   :actor-name "The Shire"
   :actor-pic *default-universe-pic*
   :scene-description "Middle Earth" 
   :the-end-of-time 10 
   :the-max-num-events 30 ))

(set-canvas-w-h! u4 
 (image-width *default-universe-pic*)
 (image-height *default-universe-pic*))

(picture-with-name u4)
(create-the-event-sources u4)
(simulate u4)
|#