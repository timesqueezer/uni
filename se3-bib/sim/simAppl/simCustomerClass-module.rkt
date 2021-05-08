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
The customers in the game
|#

(require se3-bib/sim/simBase/sim-base-package)

(defclass* customer 
  (sim-actor sim-event sim-actor-view)
  :autopred #t ; auto generate predicate queue?
  :printer  #t 
  :documentation    
  "the customers")

#|
(require se3-bib/sim/simAppl/simCustomerClass-module)
(actor-picture (make customer))
|#
