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
 ===============================================================
Module "sim-serverStations"

servers with a FIFo-waiting line and some bookkeeping statistics
on the up and downs of the number of applicants (sim-path)
 ===============================================================
|#

(provide
  pickARandomStation
  quickest-rate)

(require
  se3-bib/sim/bilder/bilder-module
  se3-bib/sim/simBase/sim-base-package
  se3-bib/sim/simAppl/simDoorClass-module
  se3-bib/sim/simAppl/simDoorImpl-module
  se3-bib/sim/simAppl/simServerStationsClass-module
  se3-bib/sim/simAppl/simServerStationsViewClass-module
  se3-bib/sim/simAppl/simCustomerClass-module)

;;; =================================================================
;;; Waiting lines: FIFO-Queues with statistics on the up and downs
;;; of the number of applicants (sim-path)
;;; =================================================================

(define (pickARandomStation) 
  ; customers pick a random service station
  (if (null? (the-actors *all-servers*))
      #f
      (random-elt 
       (the-actors *all-servers*))))

(defmethod serve 
  ((s server-station) (c customer))
  "Perform service for a customer
    at a server station"
  ; nothing to do but to schedule    the end of service event 
  (let* ((s-name (actor-name s))
         (c-name (actor-name c))
         (the-m (string-append  
                 s-name " is serving: " c-name)))
    
    (set-event-message! s the-m)
    the-m)
  )

(defmethod is-idle? ((s server-station))
  "is the station idle?"
  (empty-queue? s))

(defmethod rejecting? ((rs server-station) (c customer))
  #f; the default, serve all customers
  )

(defmethod is-rejecting? ((s server-station))
  "is the station serving somebody right now?"
  (and (not (is-idle? s))
       (rejecting? s (head-queue s))))

(defmethod is-serving? ((s server-station))
  "is the station serving somebody right now?"
  (and (not (is-idle? s))
       (not (is-rejecting? s))))

(defmethod next-event ((r-station server-station))
  "Schedule the end-of-service-event"
  (writeln "serverStation: next event");-----
  (unless (empty-queue? r-station)
    (if (rejecting? r-station (head-queue r-station))       
        (next-now-event r-station) ; send away immediately
        (next-normal-event r-station ; take time to serve
                           :sigma (event-sig r-station)))))

(defmethod request ((s server-station)(c customer))
  "customer c is requesting service at station s"    
  (let* ((s-name (actor-name s))
         (c-name (actor-name c))
         (the-m (string-append 
                 c-name 
                 " is requesting service at station " 
                 s-name))
         (idle (is-idle? s)))
    (enqueue! s c)
    (when idle
      (writeln "first request"); ----
      (serve s c); serve immediately, if queue is empty
      (next-event s)); schedule the departure event
    (set-event-message! s the-m)))

(defmethod reject 
  ((rs server-station) (rc customer))
  "Reject a customer not entitled to the service"
  ; nothing to do but to schedule the end of service event 
  (let* ((s-name (actor-name rs))
         (c-name (actor-name rc))
         (the-m (string-append  
                 (string-append
                  s-name
                  " is rejecting: " c-name))))
    
    (set-event-message! rs the-m)
    'rejecting
    ))

(defmethod handle ((station server-station))
  "handle a free-station event: customer finished,
   send the customer backstage,
   start to serve the next customer"
  ;; sofar the customers visit only one station
  ;; no dispatch to  next station is necessary
  (writeln "handle: station"); ---
  (if (empty-queue? station)
      (writeln "warning: nobody waiting at " 
               (actor-name station))
      (begin
        (departure (dequeue! station))
        ; recycle actor for the next appearance
        (if (not (empty-queue? station))
            ;serve the next customer, if somebody is waiting
            (serve station (head-queue station))))))

(defmethod enqueue! :before ((qu server-station) item)
  "Update the path statistics before entering the queue"
  (sim-path-up! qu))

(defmethod dequeue! :before ((qu server-station))
  "Update the path statistics before leaving the queue"
  (sim-path-down! qu))

(defmethod sim-init! ((station server-station))
  "Initialize  the server station for the first simulation run"
  (reset-queue! station)
  )

(defmethod sim-info ((station server-station)) 
  "request state information from the station."
  (writeln "Station: " (station-service station))
  (path-info station))

#|
 (add-method ; Variante2: direkt zur generic hinzufuegen
   initialize
   ; maintaining the list of server stations;
   ; applied after the main initialization
   (qualified-method
    :after 
    ((station server-station) args)
    (if (next-method?)
        (call-next-method station args))
    (add-actor! station *all-servers*)))
|#

(defmethod initialize :after 
  ((station server-station) args)
  ; maintaining the list of server stations;
  ; applied after the main initialization
  (add-actor! station *all-servers*))

(define (quickest-rate)
  ; the event rate of the quickest server
  (let ((theRates
         (map event-rate 
              (the-actors *all-servers*))))
    (if (null? theRates) 1.0
        (apply max theRates)
        )))

(define (test-server)
  
  (make sim-clock)
  (make sim-calendar)
  
  (begin (define a 
           (make server-station 
                 :rate 2 
                 :actorName 'server
                 :actorPic *armafix-pic*))
         (enqueue!  a (make sim-actor :actor-name "a1" ))
         (enqueue!  a (make sim-actor :actor-name "a2"))
         (enqueue!  a (make sim-actor :actor-name "a3"))
         (enqueue!  a (make sim-actor :actor-name "a4"))
         (define b (make server-station 
                         :rate 1
                         :actorName 'serverb
                         :actorPic *universe-pic*))
         (enqueue!  b (make sim-actor :actor-name "b1" ))
         (enqueue!  b (make sim-actor :actor-name "b2"))
         (enqueue!  b (make sim-actor :actor-name "b3"))
         (enqueue!  b (make sim-actor :actor-name "b4"))
         #t)
  (define c (make server-station 
                  :rate 3
                  :actor-name "serverb"
                  :actorPic *universe-pic*))
  (enqueue!  c (make sim-actor :actor-name "b1" ))
  (enqueue!  c (make sim-actor :actor-name "b2"))
  (enqueue!  c (make sim-actor :actor-name "b3"))
  (enqueue!  c (make sim-actor :actor-name "b4"))
  
  (define re (rectangle 200 64 'solid 'black))
  
  (broadcast a sim-init! :sim-class sim-event-source)
  (all-stations-picture *current-universe*)
  )

#|
(require 
 se3-bib/sim/simAppl/sim-application-package
 
 se3-bib/sim/simAppl/simDoorClass-module
 se3-bib/sim/simAppl/simDoorImpl-module
 se3-bib/sim/simAppl/simServerStationsClass-module
 se3-bib/sim/simAppl/simServerStationsImpl-module
 se3-bib/sim/simAppl/simServerStationsViewClass-module)

(make sim-clock)
(make sim-calendar)

(begin (define a 
         (make server-station 
               :rate 2 
               :actorName 'server
               :actorPic *armafix-pic*))
       (enqueue!  a (make sim-actor :actor-name "a1" ))
       (enqueue!  a (make sim-actor :actor-name "a2"))
       (enqueue!  a (make sim-actor :actor-name "a3"))
       (enqueue!  a (make sim-actor :actor-name "a4"))
       (define b (make server-station 
                       :rate 1
                       :actorName 'serverb
                       :actorPic *universe-pic*))
       (enqueue!  b (make sim-actor :actor-name "b1" ))
       (enqueue!  b (make sim-actor :actor-name "b2"))
       (enqueue!  b (make sim-actor :actor-name "b3"))
       (enqueue!  b (make sim-actor :actor-name "b4"))
       #t)
(define c (make server-station 
                :rate 3
                :actor-name 'serverb
                :actorPic *universe-pic*))
(enqueue!  c (make sim-actor :actor-name "b1" ))
(enqueue!  c (make sim-actor :actor-name "b2"))
(enqueue!  c (make sim-actor :actor-name "b3"))
(enqueue!  c (make sim-actor :actor-name "b4"))

(define re (rectangle 200 64 'solid 'black))

(broadcast a sim-init! :sim-class sim-event-source))
(allStationsPicture)
|#