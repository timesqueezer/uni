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

(provide rand-pic-priority aremorica-demo)

(require 
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simAppl/sim-application-package
  se3-bib/sim/simAremorica/aremoricaScenarioClass-module)

(defmethod rand-pic-priority ((c customer))
  (if (has-picture? c) 
      (random)
      (+ 1.0 (random))))

(defmethod make-the-door ((world aremorica-scenario))
  :documentation  "construct the source of customers"
  (make sim-door 
        :actor-name "Main Entrance, priority door"
        :rand-gen rand-pic-priority          
        ; actors with an unique picture 
        ; are enqueued in front of actors without picture
        :rate (* 2.0 (quickest-rate)))
  );abstract

(defmethod make-the-servers ((village aremorica-scenario))
  (make server-station 
        :actor-name "Miraculix"
        :actor-pic *miraculix-pic*
        :serving-pic *miraculix-pic*
        :idle-pic *miraculix-pic*
        :rate 2
        :station-service "Magic potion")
  
  (make server-station 
        :actor-name "Verleihnix"
        :actor-pic *verleihnix-pic*
        :idle-pic *verleihnix-pic* 
        :serving-pic *verleihnix-pic* 
        
        :rate 2.3
        :station-service "Fresh Fish")
  
  (make server-station 
        :actor-name "Majestix"
        :actor-pic *majestix-pic*
        :idle-pic *majestix-pic*
        :serving-pic *majestix-pic* 
        :rate 3.0
        :station-service "The Chief")
  
  (make server-station 
        :actor-name"Automatix"
        :actor-pic *automatix-pic*
        :idle-pic  *automatix-pic* 
        :serving-pic *automatix-pic*  
        :rate 1.8 
        :station-service "Weapons")
  )

(defmethod make-the-customers ((village aremorica-scenario))
  
  (make customer          
        :actor-name "Asterix"
        :actor-pic *asterix-pic*)
  
  (make customer          
        :actor-name "Obelix"
        :actor-pic *obelix-pic*)
  
  (make customer          
        :actor-name "Idefix"
        :actor-pic *idefix-pic*)
  
  (make customer          
        :actor-name "Falbala"
        :actor-pic *falbala-pic*)
  (make customer          
        :actor-name "Gutemiene"
        :actor-pic *gutemiene-pic*)
  (make customer          
        :actor-name "Armafix"
        :actor-pic *armafix-pic*)
  (make customer          
        :actor-name "Troubadix"
        :actor-pic *troubadix-pic*)
  (make customer          
        :actor-name "Norbert"
        :actor-pic  *norbert-pic*)
  (make customer          
        :actor-name "Sinfonix"
        :actor-pic   *sinfonix-pic*)
  (make customer          
        :actor-name "Lucy"
        :actor-pic   *lucy-pic*)
  (make customer          
        :actor-name "Sally"
        :actor-pic   *sally-pic*)
  (make customer          
        :actor-name "Haegar"
        :actor-pic   *haegar-pic*)
  (make customer          
        :actor-name "Devil"
        :actor-pic   *devil-pic*)
  (make customer          
        :actor-name "Tux"
        :actor-pic   *tux-pic*)
  (make customer          
        :actor-name "Cleopatra"
        :actor-pic   *cleopatra-pic*)
  (make customer          
        :actor-name "Alice"
        :actor-pic   *alice-pic*)
  (make customer          
        :actor-name "Hamlet"
        :actor-pic   *hamlet-pic*)
  
  (let ((some-more-actors 
         '("Alkoholix" "Amarfix" "Amnesix" "Augenblix" 
                       "Badefix" "Berlix" "Bossix" "Caliguliminix" 
                       "Cathedralgotix" "Diagnostix" "Florix" 
                       "Gibtermine" "Grautvornix" "Julius-Caesar"
                       "Keinentschluss" "Mac-Teefuerzweifix" 
                       "O-Fuenfuehrteefix" "Teefax" 
                       "Maestria" "Pyramidonis"
                       "Methusalix" "Maulaf" "Nixalsverdrus" 
                       "Relax"  "Rohrpostix" "Schmalzlockus" 
                       "Schweineschmalzix" "Praktifix"
                       "Sinfonix" "Spuernix" "Stupidix" "Talentix" 
                       "Tragicomix" "Tumirnix" "Vercingetorix"
                       "Wegwienix" "Yellowsubmarine")))
    (map (lambda (c)
           (make customer          
                 :actor-name c))
         some-more-actors )))

(define (aremorica-demo) 
  (run (make aremorica-scenario 
             :actor-name "Aremorica"
             :scene-description 
             "Brave gaulles in a little village in Aremorica"
             :the-end-of-time 10 
             :the-max-num-events 200)))

;For testing:
#|
(require 
   se3-bib/sim/simAppl/sim-application-package
   se3-bib/sim/simAremorica/aremoricaScenarioClass-module)

(aremorica-demo)

or, for stepwise debugging: 
(define gallia (make aremorica-scenario 
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
