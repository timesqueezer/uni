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

;;A database for predicates and clauses

(require 
  swindle/setf swindle/misc
  (all-except 
   se3-bib/tools-module
   mappend  every some last concat identity)
  se3-bib/prolog/unify
  se3-bib/pattern-matching-module)

(provide 
 *database*  <Database>
 predic-clauses <Clause>
 set-predic-clauses!
 replace-?-vars)

; primitive classes
(define <Database> <hash-table>)
(define <Clause> <list>)
(define <Relation> <list>)

(define *database*(make-hash-table))

;a hashtable of all predicates stored in the database."
; key: the predicate symbol, 
; value: a predic-structure representing the clauses

(defclass* predic () 
  (predSym  :reader theSymbol
            :initarg :thesym
            :type <symbol>
            :documentation 
            "The symbol of the predicate")
  (clauses :reader predic-clauses
           :writer set-predic-clauses!
           :initarg :theClauses
           :initvalue '()
           :type <Clause>; <list> 
           :documentation 
           "The clauses of the predicate")
  :autopred #t ; auto generate predicate predic?
  :printer  #t 
  :documentation    
  "a predicate and its clauses"
  )

(defclass* toplevel-predic (predic) 
  (clauses :initvalue (thunk (writeln "Scheme-Prolog"))
           :type <function>
           :documentation 
           "A simple function")
  :autopred #t ; auto generate predicate queue?
  :printer  #t 
  :documentation    
  "a predicate and its clauses"
  )
;; a data structure for predicates

(defgeneric* clear-db ((db <Database>)))

(defgeneric* store-predicate! 
  ((newPredi <symbol>)(db <Database>)))

(defgeneric* store-toplevel-predicate! 
  ((newPredi <symbol>) 
   (fu <function>)
   (db <Database>)))

(defgeneric* retrieve-predicate! 
  ((pred <symbol>) (db <Database>)))

(defgeneric* clear-predicate ((pred <symbol>)))

(defgeneric* clause-head ((clause <Clause>)))

(defgeneric* clause-body ((clause <Clause>)))

(defgeneric* get-clauses ((pred <top>))) 

(defgeneric* store-clause! 
  ((clause <Clause>) (db <Database>)))

(defgeneric* add-clause! 
  ((pred <symbol>)(clause <Clause>)))

(defgeneric* predicate 
  ((relation <Relation>)))    

(defgeneric* args ((x <Relation>)))

(defmethod store-predicate! 
  ((newPredi <symbol>) (db <Database>))
  ; create an empty predicate place in the database
  ; for thh symbol of the predicate
  (unless (retrieve-predicate! newPredi db)
    (let ((predi(make predic
                      :thesym newPredi ; the name of the predicate
                      )))
      (hash-table-put! db newPredi predi)
      newPredi)))  

(defmethod store-toplevel-predicate! 
  ((newPredi <symbol>) (fu <function>) (db <Database>))
  ; create a predicate place in the database
  ; for the symbol of the predicate, store the function
  (unless (retrieve-predicate! newPredi db)
    (let ((predi
           (make toplevel-predic
                 :thesym newPredi ; the name of the predicate
                 :theClauses fu; the function
                 )))
      (hash-table-put! db newPredi predi)
      newPredi)))  

(defmethod retrieve-predicate! 
  ((pred <symbol>) (db <Database>))
  ; retrieve the predicate from the database
  ; using the symbol of the predicate as key
  (hash-table-get db pred #f)) 

#|  implement the clauses |#
;; clauses are represented as (head . body) cons cells
(defmethod clause-head ((clause <Clause>)) 
  (first clause))

(defmethod clause-body ((clause <Clause>)) 
  (rest clause))

;; clauses are stored on the predicate object
(defmethod get-clauses ((pred ???))
  ; return nil if the predicate is undefined
  '())

(defmethod get-clauses ((predS <symbol>))
  ;retrieve the clauses from the symbol
  (let ((pred (retrieve-predicate! predS
                                   *database*)))
    (when (not pred) 
      (error "undefined predicate: " predS))
    (predic-clauses pred)))

(defmethod get-clauses ((predO predic))
  ;retrieve the clauses from the object
  (predic-clauses predO))

(defmethod add-clause! 
  ((predS <symbol>)(clause <Clause>))
  (let ((predO 
         (retrieve-predicate! 
          predS
          *database*)))
    (set-predic-clauses! 
     predO
     (append 
      ; preserve the order of the clauses
      (predic-clauses predO)
      (list clause)))))

#|  implement the relations |#
;; relations 
(defmethod predicate ((relation <Relation>)) 
  (first relation))

(defmethod args ((x <Relation>)) 
  "The arguments of a relation" 
  (rest x))

(define (replace-?-vars exp)
  "Replace any ? within exp with a var of the form ?123."
  (cond ((eqv? exp '?) (gensym "?"))
        ((atom? exp) exp)
        (else (cons (replace-?-vars (first exp))
                    (replace-?-vars (rest exp))))))

(defmacro* (<- &rest clause)
  ;add a clause to the data base.
  `(store-clause! 
    (replace-?-vars  (quote ,clause))
    *database*))

(defmethod store-clause!  
  ((clause <symbol>) (db <Database>))
  #t)

(define (remove-syntactical-sugar clause)
  ; remove infix oprator :-   L.D.-F.
  (if (pair? clause)
      (filter (lambda (c)
                (not (eq? ':- c)))
              clause)
      clause))

(defmethod store-clause! 
  ((clause <Clause>) (db <Database>))
  ;add a clause to the data base, 
  ;indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (predicate (clause-head clause))))      
    (amb-assert 
     (and (symbol? pred) 
          (not (variable? pred))))
    (store-predicate! pred db )
    (add-clause! 
     pred 
     (remove-syntactical-sugar clause))))
;pred))

(defmethod clear-predicate ((predS <symbol>))
  "remove the clauses for a single predicate."   
  (let ((predO 
         (retrieve-predicate! 
          predS
          *database*)))
    (set-predic-clauses! predO '())))

(defmethod clear-db ((db <Database>))
  "remove all clauses (for all predicates) from the data base."
  (hash-table-for-each 
   (singleton-value db) 
   clear-predicate))
