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
    enemies, children, and pets can't buy magic potion and weopons,

    auto provide:
      classes:  magic-potion-station  weapons-station
                no-magic-potion-allowed no-weapons-allowed
                foreigner enemy child animal very-strong-person
      generics: reject
|#


(require  
  se3-bib/sim/simAppl/sim-application-package  
  se3-bib/sim/simAremorica/aremoricaScenarioClass-module)

#|
 ================================================================
;;; A little village with specialized server stations and customers
 ================================================================
|#

(defclass* aremoricaR-scenario (aremorica-scenario)
  :documentation 
  "A little village in Aremorica with brave gaulles"
  :autopred #t ; auto generate predicate sim-scenario?
  :printer  #t 
  )

#|
 ================================================================
;;; Specialized server stations
 ================================================================
|#

(defclass* magic-potion-station (server-station)
  :documentation 
  "A station with restricted access: selling magic potion")

(defclass* weapons-station (server-station)
  :documentation 
  "A station with restricted access")

#|
 =================================================================
;;; Specialized customers
 =================================================================
|#

(defclass* no-magic-potion-allowed (customer)
  :documentation
  "Customers with no access to magic potion:
children, dogs, foreigners), and Obelix")

(defclass* no-weapons-allowed (customer) 
  :documentation "Customers with no access to weapons:
children, dogs, enemies (e.g. romans)")

(defclass* foreigner (no-magic-potion-allowed)
  :documentation "Friends and enemies of the brave gaulles")

(defclass*  enemy (no-magic-potion-allowed no-weapons-allowed)
  :documentation 
  "Enemies of the brave gaulles: romans, normans, general villains")

(defclass*  child (no-magic-potion-allowed no-weapons-allowed)
  :documentation "A class of children")

(defclass* animal (no-magic-potion-allowed no-weapons-allowed)
  :documentation "A class of pets")

(defclass* very-strong-person (no-magic-potion-allowed) 
  :documentation 
  "A very strong person who needs no magic potion")
