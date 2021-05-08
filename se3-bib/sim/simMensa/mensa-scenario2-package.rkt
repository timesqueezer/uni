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
  se3-bib/sim/simMensa/mensa-scenario-package
  se3-bib/sim/simMensa/mensaScenario2Class-module
  se3-bib/sim/simMensa/mensaScenario2Impl-module)

(provide    
 (all-from se3-bib/sim/simMensa/mensa-scenario-package) 
 (all-from se3-bib/sim/simMensa/mensaScenario2Class-module)
 (all-from se3-bib/sim/simMensa/mensaScenario2Impl-module))


;For testing:
#|
(require se3-bib/sim/simMensa/mensa-scenario2-package)
(mensa-demo-2)

oder zu debug:
(define world  (make mensa-scenario-2))
(print (create-the-event-sources world))
; hier kann man die Akteure vor dem Simulationslauf inspizieren ...
(simulate world)
|#