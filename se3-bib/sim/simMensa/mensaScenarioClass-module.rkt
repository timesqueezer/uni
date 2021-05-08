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

(require se3-bib/sim/simAppl/sim-application-package)

(defclass* mensa-scenario 
  (customer-server-scenario)
  :documentation 
  "Eine Mensa mit Essenausgabe"
  :autopred #t ; auto generate predicate sim-scenario?
  :printer  #t 
  )

;For testing:
#|
(require se3-bib/sim/simMensa/mensaScenarioClass-module)
(define dasAll (make mensa-scenario :actorName "StEllingen" ))
|#
