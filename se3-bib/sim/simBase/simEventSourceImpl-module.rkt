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
Module "simEventSourceImpl-module"
       an implementation of simEventSourceClass-module
The active scene elements that cause events to occur
=================================================================
|#

(provide 
 random-sig random-uni random-normal random-exp)

(require 
  se3-bib/sim/simBase/sim-utility-base-package
  se3-bib/sim/simBase/simActorClass-module
  se3-bib/sim/simBase/simActorImpl-module
  se3-bib/sim/simBase/queuesClass-module  
  se3-bib/sim/simBase/queuesImpl-module
  se3-bib/sim/simBase/simClockClass-module
  se3-bib/sim/simBase/simClockImpl-module
  se3-bib/sim/simBase/simEventClass-module
  se3-bib/sim/simBase/simEventImpl-module
  se3-bib/sim/simBase/simCalendarClass-module
  se3-bib/sim/simBase/simCalendarImpl-module
  se3-bib/sim/simBase/simEventSourceClass-module)


;;; In case of equally distributed arrival events 
;;; the waiting time between two sucessive events 
;;; is negative exponentially distributed 

(define (random-uni &key (range 1.0) 
                    (granularity 2147483647))
  "A random real number, unifomly distributed between 0 and range."
  (* (/ (random granularity) 
        granularity)
     range))

(define (random-sig &key (p-positive 0.5))
  "A random signum, 1 or -1"
  (if (> (- (random-uni) p-positive) 0.0)
      -1 1))

(define (random-exp &key (1/mu 1.0))
  "Return a random number, with a negative exponential distribution,
   arrival rate '1/mu'in a simulation."
  ; the higher the rate, the smaller is the interval between successive events
  (/ (- (log (random-uni)))
     1/mu))

(define (random-normal &key (mu 0.0) (sigma 1.0))
  "Return a random number, with a normal distribution,
   mean value mu, variance sigma^2."
  (let*  ([v (1- (* 2 (random-uni)))]
          [r (abs v)]
          [fact (sqrt (/ (* -2 (log r) r)))])
    (+ mu (* fact v sigma))))

(define (random-mixture
         randoms-1 frac-1 randoms-2)
  "Random numbers obeying a mixture distribution"
  (if (> (random-uni) frac-1)
      (randoms-1)
      (randoms-2)))

(define (random-normal-mixture 
         &key (mu-1 1.0) (sigma-1 1.0)  
         (mu-2 2.0) (sigma-2 1.0)
         (frac-1 0.5))
  "Random numbers obeying a mixture distribution"
  (random-mixture
   (curry random-normal :mu mu-1 :sigma sigma-1)
   frac-1
   (curry random-normal :mu mu-2 :sigma sigma-2)))

(defmethod next-event ((actor sim-event-source))
  "Default: Schedule the actor for the next exp. distributed event."
  (next-exp-event actor))

(defmethod next-event ((actor sim-const-event-source))
  "Schedule the actor for the next event 
with constant intervals between events."
  (next-const-event actor))

(defmethod next-event ((actor sim-exp-event-source))
  "Schedule the actor for the next 
exponentially distributed event."
  (next-exp-event actor))

(defmethod next-event ((actor sim-normal-event-source))
  "Schedule the actor for the next 
exponentially distributed event."
  (next-normal-event actor :sigma (event-sig actor)))

(defmethod handle :after ((actor sim-event-source))
  "After handling an event source create the next event"
  (writeln "handle: after " (actor-name actor))
  (next-event actor))


(defmethod next-now-event ((actor sim-event-source))
  "Schedule the actor for an event to take place right now."
  (inc! (event-count actor))
  (schedule actor (now)))


(defmethod next-exp-event ((actor sim-event-source))
  "Schedule the actor for the next exponentially distributed event."
  (inc! (event-count actor))
  (schedule actor
            (add-time (now)
                      (random-exp :1/mu 
                                  (event-rate actor)))))


(defmethod next-normal-event ((actor sim-event-source) &key (sigma 1))
  "Schedule the actor for the next normally distributed event."
  (inc! (event-count actor))
  (schedule actor
            (add-time (now)
                      (abs (random-normal 
                            :mu (/ 1 (event-rate actor)) 
                            :sigma sigma)))))


(defmethod next-const-event ((actor sim-event-source))
  "Schedule the actor for the next event 
with constant intervals between events."
  (inc! (event-count actor))
  (schedule actor
            (add-time (now)
                      (/ 1 (event-rate actor)))))

(defmethod sim-init! ((source sim-event-source))
  "Initialize  the event source for the first simulation run"
  (set-event-count! source 0))

(defmethod sim-info ((source sim-event-source))
  "request state information from an event source."
  (writeln "Event source: " (actor-name source)
           " Number of events: " (event-count source))
  )

;Probably deprecated
#|
 (define (histogram-of-randoms 
    &key (random-source random-uni)
          (n-events 1000)
          (n-bins 20) 
          (min-val 0.0)
          (max-val 1.0))
    (let ((hits (make-array n-bins :initial-element 0))
          (bin-size (/ (- max-val min-val) n-bins)))
      (loop for i below n-events 
            and bin = (truncate
                       (- (funcall random-source) min-val)
                       bin-size) do
                                 (when (< -1 bin n-bins)
                                   (inc! (svref hits bin))))
      hits))
  
  (define (print-histogram histo min-val max-val path)    
    (with-open-file
     (histo-stream
      path
      :direction :output
      :if-exists :supersede)
     (let ((bin-size (/ (- max-val min-val) (length histo))))
       (loop for count across histo
             and bin from (+ min-val (* 0.5 bin-size))
             to max-val by bin-size do
             (format histo-stream
                     "~a ~a~&"
                     bin count)))))
|#


;For testing
#|

(require
   se3-bib/sim/simBase/sim-utility-base-package
   se3-bib/sim/simBase/simActorClass-module
   se3-bib/sim/simBase/simActorImpl-module
   se3-bib/sim/simBase/queuesClass-module  
   se3-bib/sim/simBase/queuesImpl-module
   se3-bib/sim/simBase/simClockClass-module
   se3-bib/sim/simBase/simClockImpl-module
   se3-bib/sim/simBase/simEventClass-module
   se3-bib/sim/simBase/simEventImpl-module
   se3-bib/sim/simBase/simCalendarClass-module
   se3-bib/sim/simBase/simCalendarImpl-module
   se3-bib/sim/simBase/simEventSourceClass-module
   se3-bib/sim/simBase/simEventSourceImpl-module)

(make sim-clock )
(make sim-calendar )
(actor-name  *current-calendar*)
(define a (make sim-event-source))
|#