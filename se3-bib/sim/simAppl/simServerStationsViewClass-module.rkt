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
 idle-pic serving-pic reject-pic clerk-pic bg-pic
 set-idle-pic! set-serving-pic! set-reject-pic!
 set-clerk-pic! set-bg-pic!)

(require se3-bib/sim/bilder/bilder-module
         se3-bib/sim/simBase/sim-base-package)

(define *default-bg-pic* 
  (rectangle (image-width *default-idle-pic*) 
             (image-height  *default-reject-pic*) 
             'solid 'gray))

(defclass* server-station-view 
  (sim-actor-view)
  (clerk-picture
   :reader clerk-pic
   :writer set-clerk-pic!
   :initarg :clerk-pic
   :initvalue *default-idle-pic* 
   ;   :allocation :class
   :documentation 
   "A picture of an idle server"
   ) 
  (idle-picture
   :reader idle-pic
   :writer set-idle-pic!
   :initarg :idle-pic
   :initvalue *default-idle-pic* 
   ;   :allocation :class
   :documentation 
   "A picture of an idle server"
   )    
  (serving-picture
   :reader serving-pic
   :writer set-serving-pic!
   :initarg :serving-pic
   :initvalue *default-serving-pic* 
   ;   :allocation :class
   :documentation 
   "A picture of the current state of the actor"
   )    
  (reject-picture
   :reader reject-pic
   :writer set-reject-pic!
   :initarg :reject-pic
   :initvalue  *default-reject-pic* 
   ;   :allocation :class
   :documentation 
   "A picture of the current state of the actor"
   )
  (bg-picture
   :reader bg-pic
   :writer set-bg-pic!
   :initarg :bg-pic
   :initvalue *default-bg-pic* 
   :allocation :class
   :documentation 
   "A picture of the current state of the actor"
   )
  
  :autopred #t ; auto generate predicate queue?
  :printer  #t 
  
  :documentation 
  "The View of a server station")

(defgeneric* picture-of-clerk ((s server-station-view))
  :documentation "a picture of the state of the clerk"
  )

(defgeneric* picture-of-counter ((s server-station-view))
  :documentation 
  "Picture of clerk and an advertisement of the station service")

(defgeneric* init-clerk-bg ((s server-station-view))
  :documentation 
  "compute the dimensions of the view, create gray background plate")

(defgeneric* all-stations-picture ((sc sim-scenario))
  :documentation 
  "a picture of all the server stations and waiting lines"
  )
