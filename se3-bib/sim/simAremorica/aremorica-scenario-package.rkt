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
 (all-from se3-bib/sim/simAppl/sim-application-package)
 (all-from se3-bib/sim/simAremorica/aremoricaScenarioClass-module)
 (all-from se3-bib/sim/simAremorica/aremoricaScenarioImpl-module))

(require 
  se3-bib/sim/simAppl/sim-application-package
  se3-bib/sim/simAremorica/aremoricaScenarioClass-module
  se3-bib/sim/simAremorica/aremoricaScenarioImpl-module)


;For testing:
#|
(require se3-bib/sim/simAremorica/aremorica-scenario-package) 
(aremorica-demo)

(define obelix 
  (retrieve-by-name *current-universe* "Obelix"))
(define miraculix 
  (retrieve-by-name *current-universe* "Miraculix"))
|#
