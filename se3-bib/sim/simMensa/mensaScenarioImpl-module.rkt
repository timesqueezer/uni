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

(provide mensa-demo-1)

(require 
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simAppl/sim-application-package   
  se3-bib/sim/simMensa/mensaScenarioClass-module)

(define (make-station nam rte serv)
  (make server-station 
        :actor-name nam
        :actor-pic *default-idle-pic*
        :rate rte
        :station-service serv))

(defmethod make-the-servers ((mensa mensa-scenario))
  (make-station "Essen-1" 2   "Eintopf")
  (make-station "Essen-2" 2.3 "Pizza")
  (make-station "Essen-3" 2.3 "Currywurst")
  (make-station "Essen-4" 0.5 "Corn Dogs"))

(defmethod make-the-customers ((mensa mensa-scenario))
  (let ([some-students 
         '("Harry" "Susie" "Sally" "Paula" "Anja" 
                   "Ernie" "Bert" "Hermione" "Arnold" 
                   "Paris" "Maja" "Heidi" "Anton" 
                   "Kunigunde" "Erwin" "Otto" "Hilde" 
                   "Christoph" "Wolfgang" "Christiane" 
                   "Ronald" "Ingbert" "Karin" "Helmut"
                   "Sascha" "Dominique" "Donald" "Daisy" 
                   "Prudence" "Ansgar" "Ottilie" "Siegfried")])
    (map (lambda (c)
           (make customer          
                 :actor-name c))
         some-students )))

(define (mensa-demo-1)  
  (run (make mensa-scenario 
             :actor-name "Mensa"
             :actor-pic *default-universe-pic*
             :scene-description "Mensa StEllingen einfach" 
             :the-end-of-time 10 
             :the-max-num-events 200)))

;For testing:
#|
(require se3-bib/sim/simMensa/mensa-scenario-package) 
(mensa-demo-1)

oder:
(define world (make mensa-scenario))
(create-the-event-sources world)
    ; hier kann man die Akteure vor dem Simulationslauf inspizieren ...
(simulate world )
|#