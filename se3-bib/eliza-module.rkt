#lang racket

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(provide 
 *eliza-rules*  *eliza-simple* *eliza-fancy*  
 eliza eliza-simple eliza-fancy eliza-tool eliza-with-tool
 use-eliza-rules
 eliza-demo *elizas-voice* *patients-voice*) 

(require 
 se3-bib/tools-module
 se3-bib/demo-gui-module
 se3-bib/macos-module
 se3-bib/string-module
 se3-bib/pattern-matching-module
 (prefix-in cl: (only-in swindle some)))
 ; die Common Lisp Variante von "some" 
 ; gibt das Resultat der Funktionsanwendung 
 ; für das erste  Element der Liste zurück, 
 ; das das übergebene Semiprädikat erfüllt.


(define *eliza-simple*
  '((((?* ?x) hello (?* ?y))      
     (How do you do.  Please state your problem.))
    ((bye)(bye))
    (((?* ?x) I want (?* ?y))     
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y)) 
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))       
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))     
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))     
     (What other feelings do you have?))))

;;; ==============================

#|
; ohne cl:some mit memf
(define (transform input rule)
  (let ([bindings (pat-match 
                   (rule-pattern rule) 
                   input 
                   no-bindings)])
    (if (not (eqv? bindings fail))
        (sublis (switch-viewpoint bindings)
                (random-elt (rule-responses rule)))
        fail)))

(define (use-eliza-rules input)
  "Find some rule with which to transform the input."
  (let ([somerules (memf 
                    (lambda (rule)
                      (pat-match 
                       (rule-pattern rule) 
                       input 
                       no-bindings))
                    *eliza-rules*)])      
    (if (not (eqv? somerules fail))
        (transform input (car somerules))
        fail)))
|#

(define (use-eliza-rules input)
  "Find some rule with which to transform the input."
  (cl:some 
   (lambda (rule)
     (let ((result (pat-match 
                    (rule-pattern rule) 
                    input 
                    no-bindings)))
       (if (not (eqv? result fail))
           (sublis (switch-viewpoint result)
                   (random-elt (rule-responses rule)))
           fail)))
   *eliza-rules*))

(define (switch-viewpoint words)
  "Change I to you and vice versa, and so on."
  (sublis '((I . you) (you . I) (me . you) (am . are))
          words))
#|
> (eliza)
ELIZA> (hello there)
(HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.) 
ELIZA> (i want to test this program)
(WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM) 
ELIZA> (i could see if it works)
(DO YOU REALLY THINK ITS LIKELY THAT IT WORKS)
ELIZA> (no not really)
(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
ELIZA> (no)
(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?) 
ELIZA> (forget it-- i was wondering how general the program is)
(WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?)
ELIZA> (i felt like it)
(WHAT OTHER FEELINGS DO YOU HAVE?) 
ELIZA> (i feel this is enough)
(DO YOU OFTEN FEEL THIS IS ENOUGH ?) 
ELIZA> [Abort]
|#

(define *eliza-rules*
  '(((bye)(bye))
    (((?* ?x) hello (?* ?y))      
     (How do you do.  Please state your problem.))
    (((?* ?x) computer (?* ?y))
     (Do computers worry you?) (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))
    (((?* ?x) computers (?* ?y))
     (Do computers worry you?) (What do you think about machines?)
     (Why do you mention computers?)
     (What do you think machines have to do with your problem?))
    (((?* ?x) name (?* ?y))
     (I am not interested in names))
    (((?* ?x) sorry (?* ?y))
     (Please do not apologize) (Apologies are not necessary)
     (What feelings do you have when you apologize))
    (((?* ?x) I remember (?* ?y)) 
     (Do you often think of ?y)
     (Does thinking of ?y bring anything else to mind?)
     (What else do you remember) (Why do you recall ?y right now?)
     (What in the present situation reminds you of ?y)
     (What is the connection between me and ?y))
    (((?* ?x) do you remember (?* ?y))
     (Did you think I would forget ?y ?)
     (Why do you think I should recall ?y now)
     (What about ?y) (You mentioned ?y))
    (((?* ?x) if (?* ?y)) 
     (Do you really think its likely that ?y) (Do you wish that ?y)
     (What do you think about ?y) (Really-- if ?y))
    
    (((?* ?x) I dreamt (?* ?y))
     (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
     (Have you dreamt ?y before?))
    (((?* ?x) dream about (?* ?y))
     (How do you feel about ?y in reality?))
    (((?* ?x) dream (?* ?y))    
     (What does this dream suggest to you?) (Do you dream often?)
     (What persons appear in your dreams?)
     (Do you believe that dream has to do with your problem?))
    (((?* ?x) my mother (?* ?y))
     (Who else in your family ?y) (Tell me more about your family))
    (((?* ?x) my father (?* ?y))
     (Your father) (Does he influence you strongly?) 
     (What else comes to mind when you think of your father?))
    
    (((?* ?x) I want (?* ?y))     
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) I need (?* ?y))     
     (What would it mean if you got ?y)
     (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) I am glad (?* ?y))
     (How have I helped you to be ?y) (What makes you happy just now)
     (Can you explain why you are suddenly ?y))
    (((?* ?x) I am sad (?* ?y))
     (I am sorry to hear you are depressed)
     (I'm sure its not pleasant to be sad))
    (((?* ?x) are like (?* ?y))   
     (What resemblance do you see between ?x and ?y))
    (((?* ?x) is like (?* ?y))    
     (In what way is it that ?x is like ?y)
     (What resemblance do you see?)
     (Could there really be some connection?) (How?))
    (((?* ?x) alike (?* ?y))      
     (In what way?) (What similarities are there?))
    (((?* ?x) same (?* ?y))       
     (What other connections do you see?))
    
    (((?* ?x) I was (?* ?y))       
     (Were you really?) (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) was I (?* ?y))
     (What if you were ?y ?) (Do you think you were ?y)
     (What would it mean if you were ?y))
    (((?* ?x) I am (?* ?y))       
     (In what way are you ?y) (Do you want to be ?y ?))
    (((?* ?x) am I (?* ?y))
     (Do you believe you are ?y) (Would you want to be ?y)
     (You wish I would tell you you are ?y)
     (What would it mean if you were ?y))
    (((?* ?x) am (?* ?y))
     (Why do you say "AM?") (I don't understand that))
    (((?* ?x) are you (?* ?y))
     (Why are you interested in whether I am ?y or not?)
     (Would you prefer if I weren't ?y)
     (Perhaps I am ?y in your fantasies))
    (((?* ?x) you are (?* ?y))   
     (What makes you think I am ?y ?))
    
    (((?* ?x) because (?* ?y))
     (Is that the real reason?) (What other reasons might there be?)
     (Does that reason seem to explain anything else?))
    (((?* ?x) were you (?* ?y))
     (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
    (((?* ?x) I cannot (?* ?y))    
     (Maybe you could ?y now) (What if you could ?y ?))
    (((?* ?x) I feel (?* ?y))     
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))     
     (What other feelings do you have?))
    (((?* ?x) I (?* ?y) you (?* ?z))   
     (Perhaps in your fantasy we ?y each other))
    (((?* ?x) why don't you (?* ?y))
     (Should you ?y yourself?)
     (Do you believe I do not ?y) (Perhaps I will ?y in good time))
    (((?* ?x) yes (?* ?y))
     (You seem quite positive) (You are sure) (I understand))
    (((?* ?x) no (?* ?y))
     (Why not?) (You are being a bit negative)
     (Are you saying NO just to be negative?))
    
    (((?* ?x) someone (?* ?y))
     (Can you be more specific?))
    (((?* ?x) everyone (?* ?y))
     (surely not everyone) (Can you think of anyone in particular?)
     (Who for example?) (You are thinking of a special person))
    (((?* ?x) always (?* ?y))
     (Can you think of a specific example) (When?)
     (What incident are you thinking of?) (Really-- always))
    (((?* ?x) what (?* ?y))
     (Why do you ask?) (Does that question interest you?)
     (What is it you really want to know?) (What do you think?)
     (What comes to your mind when you ask that?))
    (((?* ?x) perhaps (?* ?y))    
     (You do not seem quite certain))
    (((?* ?x) are (?* ?y))
     (Did you think they might not be ?y)
     (Possibly they are ?y))
    (((?* ?x) bye (?* ?y))
     (See you later alligator!))
    (((?* ?x))               
     (Very interesting) (I am not sure I understand you fully)
     (What does that suggest to you?) (Please continue) (Go on) 
     (Do you feel strongly about discussing such things?))
    ))

(define *eliza-fancy* *eliza-rules*)

(define(eliza-simple)
  (set! *eliza-rules* *eliza-simple*)
  (eliza)
  )
(define(eliza-fancy)
  (set! *eliza-rules* *eliza-fancy*)
  (eliza))

(define (eliza)
  "Respond to user input using pattern matching rules."
  (display '(please enter your question as a list of symbols))
  (display "\n")
  (let ([dialog 
         (lambda (x)
           (display "eliza> ")
           (let ([input (read)])
             (writeln 
              (flatten (use-eliza-rules input)))
             input))])
    (iter-until 
     dialog 
     (curry equal? '(bye)) #f)) 
  'ciao!)

(define (get-patient)
  (display "eliza> ")
  (read))

(define (show-answer inp)
  (writeln inp))

(define (eliza-tool 
         the-rules 
         get-the-patients-remark
         ; a procedure returning a list
         show-elizas-answer
         ; a procedure accepting a list
         )
  "Respond to user input using pattern matching rules."
  (set! *eliza-rules* the-rules)
  (let ((dialog 
         (lambda (x)
           (let ((input (get-the-patients-remark)))
             (show-elizas-answer (flatten (use-eliza-rules input)))
             input))))
    (iter-until dialog (curry equal? '(bye)) #f) 'ciao!))

(define (eliza-with-tool)
  (eliza-tool
   *eliza-fancy* 
   get-patient
   ; a procedure returning a list
   show-answer
   ; a procedure accepting a list
   ))


(define *elizas-voice* Victoria)
(define *patients-voice* Fred)

(define (transform-question question-string)
  (let* ([the-question-as-list 
          (string->list-of-symbols question-string)]
         [the-answer-as-list 
          (flatten (use-eliza-rules the-question-as-list))]
         [the-answer-as-string 
          (list-of-symbols->string the-answer-as-list)])
    (*patients-voice* question-string)
    (*elizas-voice* the-answer-as-string)
    the-answer-as-string))

(define (eliza-demo)
  (dialog-gui transform-question "The doctor is in. Please come in."))

;(define nb no-bindings) ; zum Testen