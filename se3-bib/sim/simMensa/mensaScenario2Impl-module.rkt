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

(provide mensa-demo-2 veggie meat)

(require 
 se3-bib/sim/bilder/bilder-module
 se3-bib/sim/simMensa/mensa-scenario-package   
 se3-bib/sim/simMensa/mensaScenario2Class-module)

(defmethod has-picture? ((obj vegetarian))     
  "does the actor have an unique picture?"
  (not (member (actor-pic obj) 
               (list *defaultpic* *defaultRCpic*))))

(defmethod veggie ((s server-station))
  (if (member 
       (station-service s) 
       '("Corn Dogs" "Pizza"))
      (change-class! ;achtung!
       s vegetarian-food-server)))

(defmethod meat ((s server-station))
  (if (member 
       (station-service s) 
       '("Currywurst" "Eintopf"))
      (change-class!
       s general-food-server)))

(defmethod make-the-servers :after
  ((mensa mensa-scenario-2))
  (broadcast mensa veggie
             :sim-class server-station)
  (broadcast mensa meat
             :sim-class server-station)
  (remove-duplicates! *all-servers*))

(defmethod rejecting? ((s general-food-server)
                       (c vegetarian))
  #t; the default, serve all customers
  )

(defmethod serve
  ((s general-food-server)
   (c vegetarian))
  (reject s c))

(defmethod make-the-customers 
  :after ((mensa mensa-scenario-2))
  
  (let ((some-vegetarians 
         '("Demeter" "Flora" "Wurzelsepp" "Waldfee" "Donald"
                     "Rapunzel" "Rosemarie" "Tinkerbell"
                     "Winnie" "Kräuterhexe" "Persephone" 
                     "MrClou" "Fleur"
                     "Artemis" "Diana" "Hera" "Sonja" 
                     "Adonis" "Ganymed")))
    (map (lambda (c)
           (make vegetarian          
                 :actor-name c
                 :actor-pic *defaultRCpic*))
         some-vegetarians)))

(define (mensa-demo-2)
  (let ([die-mensa
         (make mensa-scenario-2 
               :actor-name "StEllingen 2"
               :scene-description 
               "Mensa mit spezialisierten Kunden "
               :the-end-of-time 10 
               :the-max-num-events 200 )])
    (run die-mensa)))

;For testing:
#|
(require se3-bib/sim/simMensa/mensaScenario2-package)

  #| (define s (make server-station 
          :actor-name "ss"
          :actor-pic *default-serving-pic*
          :rate 2
          :station-service "Eintopf"))
  |#
(mensa-demo-2)
|#
