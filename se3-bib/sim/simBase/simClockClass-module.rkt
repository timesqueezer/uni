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
Module sim-clock-module
provides: - a class simClock
=================================================================
|#

(provide 
 read-sim-clock set-sim-clock!)
 #|   
   Auto provide: sim-clock
 |#

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module)


(defclass* sim-clock (sim-actor)
  (sim-time 
   :reader read-sim-clock
   :writer set-sim-clock!
   :initvalue 0
   :type <number>
   :documentation
   "The simulation time")    
  :autopred #t ; auto generate predicate sim-clock?
  :printer  #t 
  :documentation "The simulation clock"
  )

(defgeneric* clock-info ((clock sim-clock)))


