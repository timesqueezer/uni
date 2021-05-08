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

(provide test-fifth-avenue)

(require se3-bib/sim/simAppl/sim-application-package)

(defmethod make-the-servers 
  ((world customer-server-scenario))
  (make server-station 
        :actor-name "Tiffany's"
        :rate 1.8 
        :station-service "Jewels")
  (make server-station 
        :actor-name "Gucci"
        :rate 1.0 
        :station-service "Handbags")
  (make server-station 
        :actor-name "Sack's"
        :rate 1.0 
        :station-service "Clothing") )

(defmethod make-the-customers 
  ((world customer-server-scenario))
  (make customer 
        :actor-name "Paris Hilton"
        )
  (make customer 
        :actor-name "Princess Caroline"
        )
  (make customer 
        :actor-name "Prince William"
        )
  (make customer 
        :actor-name "Steffi Graf"
        )
  (make customer 
        :actor-name "Hillary Clinton"
        ))

(define (test-fifth-avenue)
  ; go shopping on New York's famous Fifth Avenue
  (let([ fifth-Avenue 
         (make 
          customer-server-scenario ;class of the universe
          :actor-name "Fifth Avenue"
          :actor-pic *default-universe-pic*
          :scene-description "A Shopping Trip" 
          :the-end-of-time 10 
          :the-max-num-events 30 )])
    
    (run fifth-Avenue)
    ))

(test-fifth-avenue)