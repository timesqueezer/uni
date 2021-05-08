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

;A wrapper package for the mensa scenario

(provide 
 (all-from se3-bib/sim/simAppl/sim-application-package)   
 (all-from se3-bib/sim/simMensa/mensaScenarioClass-module)
 (all-from se3-bib/sim/simMensa/mensaScenarioImpl-module))

(require 
  se3-bib/sim/simAppl/sim-application-package
  se3-bib/sim/simMensa/mensaScenarioClass-module 
  se3-bib/sim/simMensa/mensaScenarioImpl-module)  

;For testing
#|
(require se3-bib/sim/simMensa/mensa-scenario-package) 
(mensa-demo-1)
|#