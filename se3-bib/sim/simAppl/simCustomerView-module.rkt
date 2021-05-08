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

(require 
  se3-bib/sim/simBase/sim-base-package        
  se3-bib/sim/simAppl/simCustomerClass-module
  se3-bib/sim/simAppl/simCustomerImpl-module)

(defmethod actor-picture 
  ((c customer))
  (picture-with-name c))