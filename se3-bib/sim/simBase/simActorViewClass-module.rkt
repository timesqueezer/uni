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
  sim-actor-view: A mixin class for simulation objects,
  - provides a framework to create a view of the active scene components;
  - some utility functions to create images
 =================================================================
|#

(provide 
 ; accessors
 set-actor-pic! actor-pic actor-movie set-actor-movie!
 canvas-width canvas-height  bg-screen name-fontsize
 ; global variables
 *default-view* ; a delegate to access static fields
 
 ; some sample pictures
 *defaultpic* 
 *defaultRCpic* 
 *default-universe-pic*
 *clock+cal-pic*)

(require 
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simBase/sim-utility-base-package)  

(define *defaultpic* *yellow-smiley-pic*); a default picture for actors
(define *defaultRCpic* *green-smiley-pic*); a default picture for special actors 
(define *default-universe-pic* *earth-pic*)
(define *universe-small-pic* *universe-pic*)
(define *clock+cal-pic* *clock-and-calendar-pic*)


(define *default-canv-width* 1000)
(define *default-canv-height* 750)
(define *default-name-fontsize* 14)

(defclass* sim-actor-view ()  
  ; dynamic fields
  (actor-pic
   :reader actor-pic
   :writer set-actor-pic!
   :initarg :actor-pic
   :initvalue *defaultpic* 
   :documentation 
   "A picture of the current state of the actor"
   )
  (actor-movie
   :reader actor-movie
   :writer set-actor-movie!
   :type <list>
   :initvalue '() 
   :documentation 
   "A list of pictures of the current state of the actor")
  
  ; static fields
  ; the dimensions of the canvas
  (canvas-width 
   :allocation :class
   :reader canvas-width
   :writer set-canvas-width!
   ;   :initarg :canvas-w ; does not work with static fields
   ; initialize with set-canvas-w-h!
   :initvalue  *default-canv-width*;
   :documentation 
   "A picture of the current state of the actor"
   )
  
  (canvas-height 
   :allocation :class
   :reader canvas-height
   :writer set-canvas-height!
   ;   :initarg :canvas-h ; does not work with static fields
   ; initialize with set-canvas-w-h!
   :initvalue *default-canv-height* 
   :documentation 
   "A picture of the current state of the actor"
   )
  
  (bg-screen 
   :allocation :class
   :reader bg-screen
   :writer set-bg-screen!
   :initvalue 
   (empty-scene *default-canv-width* *default-canv-height*))
  
  (name-fontsize 
   :allocation :class
   :reader name-fontsize
   :writer set-name-fontsize!
   :initvalue *default-name-fontsize* 
   :documentation 
   "A picture of the current state of the actor"
   )
  
  :autopred #t ; auto generate predicate sim-actor-view?
  :printer  #t    
  :documentation "A mixin class for simulation objects,
    provides a framework for creating pictures of all actors"
  )

(defgeneric* has-picture? ((av sim-actor-view))
  :documentation 
  "does the actor have an unique picture or only a default picture?"
  )

(defgeneric* actor-picture ((av sim-actor-view))
  :documentation    
  "Abstract: return a picture of the actor's state"
  )

(defgeneric* picture-with-name ((av sim-actor-view))
  ; a picture of the actor with the name below
  ; on a gray background
  )
(defgeneric* set-canvas-w-h! ((av sim-actor-view) w h))

; these methods need to access private fields of the class
; and are defined together with the definition of the fields
; set the width and height of the canvas
; and the empty background screen

(defmethod set-canvas-w-h! ((av sim-actor-view) w h)
  ; set the width and height of the canvas
  ; and the empty background screen
  (set-canvas-width! av w)
  (set-canvas-height! av h)
  (set-bg-screen! av (empty-scene w h))
  )

(define *default-view* (make sim-actor-view))
; a default object to acces the static fields

#|
(define av1 (make sim-actor-view))
(print av1)
(define av2 (make sim-actor-view :actor-pic *default-universe-pic* ))
(print av2)(actor-pic av2)
(define av3 (make sim-actor-view :actor-pic *defaultRCpic*))
(print av3)
(actor-pic av3)
(set-canvas-w-h! av2 40 50)
(print av3)
|#