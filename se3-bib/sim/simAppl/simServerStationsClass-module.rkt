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
 =================================================================
Module "sim-serverStations"

servers with a FIFo-waiting line and some bookkeeping statistics
on the up and downs of the number of applicants (sim-path)
 =================================================================
|#

(provide 
 station-service
 *all-servers*
 
 #| auto provide:
   classes: server-station
   generics: serve request         
 |#
 )

(require
  se3-bib/sim/simBase/sim-base-package   
  se3-bib/sim/simAppl/simServerStationsViewClass-module
  se3-bib/sim/simAppl/simCustomerClass-module)

(define *all-servers* (make set-of-actors))
; A set of all server stations, set by initialize

(defclass* server-station 
  (sim-normal-event-source 
   FIFO-queue sim-path server-station-view)
  (service 
   :reader station-service
   :initvalue "Super Market"
   :initarg :station-service
   :type <string>)
  
  :autopred #t ; auto generate predicate queue?
  :printer  #t 
  
  :documentation 
  "A FIFO-queue with statistics 
     on the ups and downs of the queue length")

;;; ========================================================
;;; Waiting lines: 
;;; FIFO-Queues with statistics on the up and downs
;;; of the number of applicants (sim-path)
;;; ========================================================

(defgeneric* serve ((s server-station)(c customer))
  :documentation "serve customer c at station s")

(defgeneric* request ((s server-station)(c customer))
  :documentation "customer c is requesting service at station s") 

(defgeneric* reject 
  ((server-station)(c customer))
  :documentation "do not serve customer c at station s")

(defgeneric* is-idle? ((s server-station))
  :documentation "is the station idle?")

(defgeneric* is-serving? ((s server-station))
  :documentation "is the station serving somebody right now?")

(defgeneric* is-rejecting? ((s server-station))
  :documentation "is the station rejecting somebody right now?") 

(defgeneric* rejecting? ((rs server-station) (c customer))
  :documentation "will customer c be served at station s?")



