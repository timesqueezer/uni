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
  se3-bib/sim/simBase/sim-utility-base-package    
  se3-bib/sim/simBase/queuesClass-module  
  (all-except scheme/mpair mappend) ; use mutable lists
  test-engine/scheme-tests)

(defmethod head-queue ((qu queue))
  "Return the item at the front of  the queue." 
  (sm-first-element (queue-items qu)))

(defmethod empty-queue? ((qu queue))
  "Is the queue empty?" 
  (sm-empty? (queue-items qu)))

(defmethod reset-queue! ((qu queue)) 
  "Reset the queue, remove all items"
  (set-queue-items! qu (sm-makelist)))

(defmethod enumerate-queue ((qu queue))    
  "Return a list of  the queue-items"
  (sm->list (queue-items qu)))

(defmethod dequeue! ((qu queue))
  "Remove an item from the front 
  of the queue. Return the item."
  (sm-pop! (queue-items qu)))

(defmethod enqueue-items!  ((qu queue) (items <list>)) 
  "Push all items of a list onto the queue."
  (map (curry enqueue! qu) items)
  qu)    

(defmethod enqueue! ((qu LIFO-queue) item)
  "Push an item onto the queue."
  (sm-push!(queue-items qu) item)
  qu)    

(defmethod enqueue! ((qu FIFO-queue) item)
  "Insert an item at the tail of the queue."
  (sm-insert-at-back! (queue-items qu) item)
  qu)

#|
(defmethod reset-queue! :after ((qu FIFO-queue))
    :documentation 
    "Reset the queue, remove all items"
    (if null? (queue-items qu) 
        (set-FIFO-queue-tail! qu '{})))
 (defmethod dequeue! :after ((qu FIFO-queue))
    "Remove an item from the front 
  of the queue. Return the item."
    (if null? (queue-items qu) 
        (set-FIFO-queue-tail! qu '{})))
|#

(defmethod rLess? ((qu queue))
  ;rLess?: queue -> (<top><top> -> boolean)
  "Generate a less?-predicate according to rank-p"
  (lambda (item1 item2)
    (< ((the-rank-p qu) item1) 
       ((the-rank-p qu) item2))))

(defmethod enqueue! ((qu RANKED-queue) item)
  "Insert an item according to the rank."
  (sm-insert-Ranked! 
   (queue-items qu) item (rLess? qu))
  qu)

#|
(defmethod randomLess? ((qu queue))
    ;randomLess?: queue -> (<top><top> -> boolean)
    "Generate a less?-predicate according to randGen"
    (lambda (item1 item2)
      (< ((the-random-gen qu) item1) 
         ((the-random-gen qu) item2))))
  
  (provide randomLess?)
|#

(defmethod enqueue! ((qu RANDOM-queue) item)
  "Insert an item at a random Position."
  (sm-insert-at-pos!
   (queue-items qu) 
   item 
   (random ; to do: (theRandomGen qu) 
    (max 1 (sm-length (queue-items qu)))))
  qu)

(check-expect 
 (let ((lifo (make LIFO-queue)))
 
   (begin (enqueue! lifo 1)
     
          (enqueue! lifo 2)
      
          (enqueue! lifo 3)
       
          (list  (dequeue! lifo)
  
                 (dequeue! lifo)
  
                 (dequeue! lifo))))
 '(3 2 1))

(check-expect 
 (let ((fifo (make FIFO-queue)))
 
   (begin (enqueue! fifo 1)
               
          (enqueue! fifo 2)
            
          (enqueue! fifo 3)
        
          (list  (dequeue! fifo)
        
                 (dequeue! fifo)
     
                 (dequeue! fifo))))
 
 '(1 2 3))

(check-expect 
 (let ((ranked (make RANKED-queue :rank-p id)))
   
   (begin (enqueue! ranked 3)
            
          (enqueue! ranked 1)
        
          (enqueue! ranked 2)
     
          (list  (dequeue! ranked)
   
                 (dequeue! ranked)
 
                 (dequeue! ranked)))) 
 '(1 2 3))

(check-expect 
 (let ((ran (make RANDOM-queue)))
 
   (begin (enqueue! ran 3)
        
          (list  (dequeue! ran))))
 '(3))

(check-expect (empty-queue? (make LIFO-queue))
              #t)

#|
(require
   se3-bib/sim/simBase/sim-utility-base-package    
   se3-bib/sim/simBase/queuesClass-module)

(print (subclass? FIFO-queue queue))
(class-slots FIFO-queue)
|#