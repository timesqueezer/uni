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
  sim-actor A mixin class for simulation objects,
  provides a framework for
      - broadcasting messages to all actors
      -initializing and resetting all actors
      - displaying a picture of the actors
=================================================================
|#

(provide 
 ; accessors
 actor-num actor-name sim-actor?
 the-actors set-the-actors! the-last-num
 ; global variables
 *the-number-generator* *all-actors*
 )
#|
  auto provide 
  classes: sim-actor 
  generics: initialize
  sim-init! sim-start sim-info  
   tell broadcast broadcast-some
  map-actors
  add-actor!
|#

(require se3-bib/sim/simBase/sim-utility-base-package)

(defclass* actor-numbers ()
  (lastnum :initvalue 0
           :accessor the-last-num
           :type <integer>
           :allocation :class)
  :autopred #t ; auto generate predicate sim-actor?
  :printer  #t 
  :documentation "a generator for unique actor numbers"
  )

(define *the-number-generator* 
  (make actor-numbers))

(defgeneric* next-actor-number ((ng actor-numbers))
  :documentation "generate a unique actor number"
  :method (method 
           ((ng actor-numbers))
           (inc! (the-last-num ng))
           (the-last-num ng)))

(defmethod next-actor-number 
  ((ng actor-numbers))
  (inc! (the-last-num ng))
  (the-last-num ng))

(defclass* sim-actor ()
  (actor-name 
   :reader actor-name
   :initvalue "anonymous"
   :initarg :actor-name
   :type <string>
   :documentation "The name of the actor"
   )
  (actor-num
   :reader actor-num
   :initializer 
   (lambda () 
     (next-actor-number 
      *the-number-generator*))
   :type <integer>
   :documentation 
   "A picture of the current state of the actor"
   )
  :autopred #t ; auto generate predicate sim-actor?
  :printer  #t 
  
  ;:documentation "A mixin class for simulation objects,
  ;provides a framework for broadcasting messages to all actors"
  )

(defclass* set-of-actors ()
  (theActors
   :accessor the-actors
   :writer set-the-actors!
   :initvalue '()
   :type <list>
   :documentation 
   "A set of actors"
   )
  :autopred #t ; auto generate predicate set-of-actors?
  :printer  #t 
  )

(define *all-actors* (make set-of-actors))

(defgeneric* add-actor! ((a sim-actor) (s setOfActors))
  :documentation 
  "add an actor to the set of all actors"
  )

(defgeneric* remove-duplicates! ((s set-of-actors))
  :documentation 
  "remove duplicate entries"
  )

(defgeneric* sim-init! ((obj sim-actor))
  :documentation 
  "further initializations after all actors have been created"
  :combination generic-begin-combination)

(defgeneric* sim-start ((obj sim-actor))
  :documentation 
  "Trigger the start-up actions, get the system going"
  :combination generic-begin-combination)

(defgeneric* sim-info ((obj sim-actor))
  :documentation 
  "request state information."
  :combination generic-begin-combination)


;;; ===========================================================
;;; Message passing
;;; ===========================================================

(defgeneric* tell ((obj sim-actor) (message <function>))
  :documentation 
  "Pass message to actor. 
   The message must be the name 
   of an applicable method or procedure."
  )

(defgeneric* broadcast 
  ((actor sim-actor) 
   (message <function>) 
   &key (sim-class sim-actor))
  :documentation 
  "Broadcast a message to all actors of a subclass of sim-class"
  )

(defgeneric* broadcast-some 
  ((actor sim-actor) 
   (message <function>) 
   &optional (sim-type sim-actor))
  :documentation 
  "Send message to actors of type sim-type,
    and find some actor who confirms the message
    by answering 'not #f'."
  )

(defgeneric* retrieve-by-name
  ((actor sim-actor) 
   (name <string>) 
   &optional (sim-class sim-actor))
  :documentation 
  "Return an actor of type sim-type,  whose actor-name is 'name'.")

(defgeneric* map-actors 
  ((actor sim-actor) 
   (message <function>) 
   &optional (sim-class sim-actor))
  :documentation 
  "Send message to actors of type sim-type,
and collect the results in a list")

(defgeneric* longest-name
  ((actor sim-actor) 
   &optional (sim-class sim-actor))
  :documentation 
  "Send message to actors of type sim-type,
and collect the longest name")
