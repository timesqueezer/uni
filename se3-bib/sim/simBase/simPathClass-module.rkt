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
Mixin class for server stations,
provides statistics on the waiting time in a queue
=================================================================
|#

(provide 
 ;slots
 items-w set-items-w!
 set-wt! wt
 tlu  set-tlu!
 ts)
 #| Auto provides:
      sim-path 
      sim-path-up sim-path-down sim-path-mean 
 |#

(require se3-bib/sim/simBase/sim-utility-base-package)  

(defclass* sim-path ()
  (n-items-waiting 
   :accessor items-w
   :initvalue 0
   :type <integer>
   :documentation 
   "The number of items in the associated queue")
  (accumulated-waiting-time
   :accessor wt
   :initvalue 0
   :type <number>
   :documentation 
   "Accumulated waiting time since the creation of the queue")
  (time-of-start
   :accessor ts
   :initvalue 0
   :type <number>
   :documentation 
   "Creation time of the queue")
  (time-of-last-update
   :type <number>
   :accessor tlu
   :initvalue 0)
  :autopred #t ; auto generate predicate sim-path?
  :printer  #t 
  :documentation 
  "Statistics on the ups and downs of the queue length")

(defgeneric* sim-path-up! ((sim-p sim-path))
  :documentation 
  "Update the statistics for a new arrival in the queue"
  :combination generic-begin-combination)


(defgeneric* sim-path-down! ((sim-p sim-path))
  :documentation 
  "Update the statistics for a departure from the queue"
  :combination generic-begin-combination)

(defgeneric* path-info ((sim-p sim-path))
  :documentation 
  "request state information from path of a queue."
  )

(defgeneric* sim-path-mean ((sim-p sim-path))
  :documentation 
  "The average waiting time of the associated queue")


