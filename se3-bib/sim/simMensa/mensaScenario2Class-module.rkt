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
    
    vegetarians, servers for vegetarian food,

    auto provide:
      classes:  magic-potion-station  weapons-station
                no-magic-potion-allowed no-weapons-allowed
                foreigner enemy child animal very-strong-person
      generics: reject
|#

(provide reject-pic 
         set-reject-pic!)

(require 
  se3-bib/sim/simAppl/sim-application-package
  se3-bib/sim/simMensa/mensaScenarioClass-module)

#|
================================================================
;;; Specialized server stations
================================================================
|#

(defclass* mensa-scenario-2 
  (mensa-scenario)
  :documentation 
  "A mensa with unfriendly servers"
  :autopred #t ; auto generate predicate sim-scenario?
  :printer  #t)

(defclass* general-food-server (server-station)
  :documentation 
  "A station with restricted access: serving menues with meat")

(defclass* vegetarian-food-server (server-station)
  :documentation 
  "A station with restricted acces: serving menues without meat")

#|
=================================================================
;;; Specialized customers
=================================================================
|#

(defclass* vegetarian (customer)
  :documentation
  "Guests who don't like meat")