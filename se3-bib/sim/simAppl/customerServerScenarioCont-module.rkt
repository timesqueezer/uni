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
  se3-bib/sim/simBase/sim-base-package
  se3-bib/sim/simAppl/simDoorClass-module 
  se3-bib/sim/simAppl/simServerStationsClass-module
  se3-bib/sim/simAppl/simCustomerClass-module
  se3-bib/sim/simAppl/customerServerScenarioClass-module
  se3-bib/sim/simAppl/simDoorImpl-module
  se3-bib/sim/simAppl/simServerStationsImpl-module)

#| Controller Methoden |#

(defmethod make-the-servers 
  ((world customer-server-scenario))
  (writeln "make-the-servers abstract"))

(defmethod make-the-door 
  ((world customer-server-scenario))
  "construct the source of customers"      
  (make sim-door 
        :actorName "Main Entrance"
        :rate (* 0.2 (quickest-rate))
        )
  ;produce events slightly slower than the quickest server
  )

(defmethod make-the-customers 
  ((world customer-server-scenario))
  (writeln "make-the-customers abstract"))

(defmethod 
  create-the-event-sources 
  ((world customer-server-scenario))
  "create the server stations and customers"
  
  (print " create-the-event-sources: customer server scenario ") 
  
  (make-the-servers world)
  
  (make-the-door world)
  ;produce events faster than the quickest server
  
  (make-the-customers world))



