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
The implementation of the class sim-actor
 A mixin class for simulation objects,
  provides a framework for
      - broadcasting messages to all actors
      - initializing and resetting all actors
      - displaying a picture of the actors
=================================================================
|#

(require 
  se3-bib/sim/simBase/sim-utility-base-package   
  se3-bib/sim/simBase/simActorClass-module)

(defmethod equals? 
  ((a sim-actor) (b sim-actor))
  (= (actor-num a)(actor-num b)))


(defmethod add-actor! ((a sim-actor) (s set-of-actors))
  ; add an actor to the list of all actors
  (push! a (the-actors s)))

(define (remove-dupl! xs)
  (if (null? xs) '()
      (if (some (curry equals? (car xs)) (cdr xs)) 
          (remove-dupl! (cdr xs))
          (cons (car xs) (remove-dupl! (cdr xs))))))

(defmethod remove-duplicates! ((s set-of-actors))
  "remove duplicate entries"
  ; necessary , if change class is used on an actor
  ; then initialize will be called again
  (set-the-actors! s (remove-dupl! (the-actors s)))
  )

(defmethod  initialize
  ;maintaining the list of actors;
  ; applied after the main initialization
  
  :after 
  ((obj sim-actor) args)
  (add-actor! obj *all-actors*))

#|
(add-method 
   initialize
   ;maintaining the list of actors;
   ; applied after the main initialization
   (qualified-method
    :after 
    ((obj sim-actor) args)
    (writeln "initialize after")
    (add-actor! obj *all-actors*)))
|#

;;; ===========================================================
;;; Message passing
;;; ===========================================================

(defmethod sim-info ((obj sim-actor))
  (writeln (actor-num obj) ": " 
           (actor-name obj) " " (class-of obj)))

(defmethod sim-start ((obj sim-actor)) 
  "Trigger the start-up actions, get the system going"
  (writeln "sim-start!: " (actor-name obj) " " (class-of obj))
  
  #t)

(defmethod sim-init! ((obj sim-actor)) 
  "further initializations after all actors have been created"
  (writeln "sim-init!: " (actor-name obj) " " (class-of obj))
  #t)

(defmethod tell 
  ((a sim-actor) (message <function>))
  ;tell: sim-actor, (sim-actor -> any) -> any
  "Pass message to actor. 
   The message must be the name 
   of an applicable method or procedure."
  (message a))

(defmethod broadcast 
  ((actor sim-actor) 
   (message <function>) 
   &key (sim-class sim-actor))
  "Broadcast message to all actors of type sim-type"
  (dolist (actr (reverse  (the-actors *all-actors*)))
          (when (instance-of? actr sim-class)
            (tell actr message)))
  #t)

(defmethod broadcast-some 
  ((actor sim-actor) 
   (message <function>) 
   &optional (sim-class sim-actor))
  "Send message to actors of class/subclass sim-class,
and find some actor who confirms the message by answering 'not #f'."
  (some 
   (lambda 
       (actr)
     (if (and 
          (subclass? (class-of actr) sim-class) 
          (tell actr message))
         actr #f))
   (the-actors *all-actors*)))


(defmethod retrieve-by-name
  ((actor sim-actor) ; z.b.*current universe*
   (name <string>) 
   &optional (sim-class sim-actor))
  :documentation 
  "Return an actor of type sim-type,  whose actor-name is 'name'."
  (broadcast-some actor 
                  (lambda (a)
                    (string=? (actor-name a) name))
                  sim-class))

(defmethod map-actors 
  ((actor sim-actor) 
   (message <function>) 
   &optional (sim-class sim-actor))
  "Send message to actors of type sim-type,
and collect the results in a list"
  (map message 
       (filter (lambda (actr)
                 (instance-of? actr sim-class ))
               (the-actors *all-actors*))))

(defmethod longest-name
  ((actor sim-actor) 
   &optional (sim-class sim-actor))
  :documentation 
  "Send a message to actors of type sim-type,
and collect the longest name"
  (foldl 
   (lambda (a b)
     (if (string<? a b) b a))
   "" 
   (map-actors actor actor-name sim-class))
  
  )

(defmethod actor-name
  ((actor <top>))
  "default name for objects of an unknown class"
  "no actor")

#|
(require se3-bib/sim/simBase/simActorImpl-module)

(make sim-actor :actor-name "Peter")
(make sim-actor :actor-name "Paul")
(make sim-actor :actor-name "Kim")

(broadcast (make sim-actor) 
           sim-info)
(broadcast-some (make sim-actor) 
                sim-actor? )

(longest-name (make sim-actor) sim-actor)

(print  (retrieve-by-name (make sim-actor) "Peter"))
|#

