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

#|
The customers in the game
|#

(require
  se3-bib/sim/simBase/sim-base-package  
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simDoorImpl-module
  se3-bib/sim/simAppl/simServerStationsClass-module
  se3-bib/sim/simAppl/simServerStationsImpl-module
  se3-bib/sim/simAppl/simCustomerClass-module)

(defmethod handle ((c customer))
  "handle a customer event: pick a service station,
      enqueue the customer at the station, close the door."
  ;; sofar the customers visit only one station
  ;; no dispatch to  next station is necessary
  
  (let ((station (pickARandomStation) ))
    (if (not station)
        (begin 
          (writeln "no station available")
          (send-backstage c))
        (request station c )))
  (close-the-door *the-sim-door*))

(defmethod sim-init! ((c customer))
  "go to the waiting line behind the door and wait for an appearance"
  (send-backstage c)
  )

#|
(add-method 
    initialize
    ; maintaining the list of server stations;
    ; applied after the main initialization
    (qualified-method
     :after ((c customer) args)
     (writeln "backstage 1" (actor-name c))
     (send-backstage c)
          (if (next-method?)
              (call-next-method c args))))
|#

#|
(make sim-clock)
(make sim-calendar)
(make sim-door)
(make customer)
(make customer)
 (broadcast (make customer) sim-init! 
            :sim-class sim-event-source)
|#