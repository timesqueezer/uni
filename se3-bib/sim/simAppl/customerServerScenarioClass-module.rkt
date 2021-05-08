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

#| set up a customer-server scenario 
      - a simulation universe (clock and calendar),
      - server stations and customers,
      - a door producing random customers

      - provides an implementation of "draw-world" 
        for customerServerScenarios
      - provides an implementation of "createTheEventSources"
        for customerServerScenarios
|#

(require 
  se3-bib/sim/simBase/sim-base-package
  se3-bib/sim/simAppl/simDoorClass-module 
  se3-bib/sim/simAppl/simServerStationsClass-module
  se3-bib/sim/simAppl/simCustomerClass-module)

(defclass* customer-server-scenario 
  (sim-scenario)
  :documentation 
  "A world with server stations and customers"
  :autopred #t ; auto generate predicate sim-scenario?
  :printer  #t 
  )


(defgeneric* make-the-customers ((world customer-server-scenario))
  :documentation  "construct the eager customers"
  );abstract

(defgeneric* make-the-door ((world customer-server-scenario))
  :documentation  "construct the source of customers"
  );abstract

(defgeneric* make-the-servers ((world customer-server-scenario))
  :documentation  "construct the friendly attendants"
  );abstract
