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
======================================================================
The implementation of simPathClass, a mixin class for server stations
provides:  statistics on the waiting time in a queue
======================================================================
|#

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module
  se3-bib/sim/simBase/simPathClass-module)

(defmethod sim-path-up! ((sim-p sim-path))
  "Update the statistics for a new arrival in the queue"
  (inc! (wt sim-p)  ;update accumulated waiting time
        (* (items-w sim-p)
           (- (now) (tlu sim-p))))
  (inc! (items-w sim-p))
  (setf! (tlu sim-p) (now))
  )

(defmethod sim-path-down! ((sim-p sim-path))
  "Update the statistics for a departure from the queue"
  (inc! (wt sim-p)  ;update accumulated waiting time
        (* (items-w sim-p)
           (- (now) (tlu sim-p))))
  (dec! (items-w sim-p))
  (setf! (tlu sim-p) (now))
  )

(defmethod sim-path-mean ((sim-p sim-path))
  "The average waiting time of the associated queue"
  (let ([delta-t (- (now) (ts sim-p))])
    (if (> delta-t 0)
        (begin 
          (inc! (wt sim-p)  ;update accumulated waiting time
                (* (items-w sim-p)
                   (- (now) (tlu sim-p))))
          (setf! (tlu sim-p) (now))
          (/ (wt sim-p) (- (now) (ts sim-p))))
        0)))


(defmethod path-info ((sim-p sim-path));  progn
  "request state information from path of a queue."
  
  (writeln " The average waiting time: "
           (sim-path-mean sim-p))
  (writeln "Number of clients still waiting: " 
           (items-w sim-p)))

;For testing
#|
(make sim-clock)
(define p (make sim-path))
(sim-path-up! p)
(sim-path-down! p)
|#