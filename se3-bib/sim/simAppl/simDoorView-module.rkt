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

(provide 
  show-empty-door
  show-person-in-door)

(require
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simBase/sim-base-package 
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simDoorImpl-module   
  se3-bib/sim/simAppl/simCustomerClass-module
  se3-bib/sim/simAppl/simCustomerImpl-module
  se3-bib/sim/simAppl/simCustomerView-module)

; show the door
(define *door-icon* *door-pic*) 
(define *door-border* 3)
(define *door-width* 
  (+ (image-width *door-icon* ) (* 2 *door-border*)))
(define *door-height* 
  (+ (image-height *door-icon* ) (* 2 *door-border*)))
(define *door-bg* (rectangle 
                   *door-width*
                   *door-height*
                   'solid
                   'white))  

(defmethod show-empty-door ((d sim-door))
  (let ([theIm 
         (overlay *door-icon* *door-bg*)])
    (set-actor-pic! d theIm)
    theIm))

(defmethod show-person-in-door ((d sim-door))
  (let ((theIm 
         (if (door-empty? d)
             (show-empty-door d)
             (overlay
              (actor-picture (theActorInTheDoor d))
              (show-empty-door d)
              ))))
    (set-actor-pic! d theIm)
    theIm))

(defmethod actor-picture 
  ((d sim-door))
  (show-person-in-door d))  
