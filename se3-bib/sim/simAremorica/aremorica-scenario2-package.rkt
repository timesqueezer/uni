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

#|Specialized customer classes and server classes:
    not everybody is entitled to buy magic potion and weapons -
    customers may be rejected by the server.
    enemies, children, and pets can't buy magic potion and weapons.

    auto provide:
      classes:  magic-potion-station  weapons-station
                no-magic-potion-allowed no-weapons-allowed
                foreigner enemy child animal very-strong-person
      generics: reject
|#

(provide 
 (all-from se3-bib/sim/simAremorica/aremorica-scenario-package )   
 (all-from se3-bib/sim/simAremorica/aremoricaScenario2Class-module)
 (all-from se3-bib/sim/simAremorica/aremoricaScenario2Impl-module))

(require  
  se3-bib/sim/simAremorica/aremorica-scenario-package  
  se3-bib/sim/simAremorica/aremoricaScenario2Class-module
  se3-bib/sim/simAremorica/aremoricaScenario2Impl-module)


;For testing:
#|
(require se3-bib/sim/simAremorica/aremorica-scenario2-package)
(aremorica-demo-2)

;or, for stepwise debugging: 
(define gallia (make aremoricaR-scenario 
               :actor-name "Aremorica"
               :scene-description 
               "Brave gaulles in a little village in Aremorica"
               :the-end-of-time 10 
               :the-max-num-events 200))
(create-the-event-sources gallia)
    ; hier kann man die Akteure vor dem Simulationslauf inspizieren ...
(define obelix 
  (retrieve-by-name *current-universe* "Obelix"))
(print obelix)
(define miraculix 
  (retrieve-by-name *current-universe* "Miraculix"))
(print miraculix)

(simulate gallia )
|#