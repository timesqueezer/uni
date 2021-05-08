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

(provide demo-with-numeric-params
         demo-without-params
         dialog-gui) 

(require  htdp/gui
          se3-bib/tools-module)

(define *demoWindow* #f); is set, when the main window is created

(define  (hide-the-main-window event)
  (hide-window *demoWindow*))

(define (the-query-string entry) 
  (car entry))

(define (the-action entry)
  (cadr entry))

(define (readTheParamsAndPerformTheDemoSelected
         demos-available
         thechoices 
         theParamsQuery 
         max-iterations
         event)
  (let* ((index (choice-index thechoices))
         (entry (list-ref demos-available index ))
         ; decode the selection index and call the procedure
         (action (the-action entry))
         (theParam (string->number(text-contents theParamsQuery))))
    
    (if (and (number? theParam) (<= 1 theParam max-iterations))
        (action theParam)
        (display "illegal argument, try again")
        )
    #t))

(define (ask-for-params
         answer-gui-item
         query-string mini maxi)
  (let ((theQuestion 
         (string-append query-string
                        " (" 
                        (number->string mini) "-" 
                        (number->string maxi) "):")))
    (draw-message answer-gui-item theQuestion)))

(define (demo-with-numeric-params 
         demos-available
         param-query-string
         max-iterations
         )
  (let* ((theDemos (map the-query-string demos-available))
         (theQuery (make-message "Please select a demo:"))
         (theChoices (make-choice theDemos))
         (theParamsQueryField (make-text (make-string 60 #\SPACE))))
    (set! *demoWindow*
          (create-window
           (list (list theQuery ; list of choices
                       theChoices)
                 (list theParamsQueryField ; list of buttons
                       (make-button
                        "o.k." ; button string
                        
                        #|(curry ; event handler
                          ;bug curry not allowed
                           readTheParamsAndPerformTheDemoSelected
                           demos-available
                           theChoices
                           theParamsQueryField
                           max-iterations)))|#
                        (lambda (event)
                          (readTheParamsAndPerformTheDemoSelected
                           demos-available
                           theChoices
                           theParamsQueryField
                           max-iterations event))))
                 (list (make-button "QUIT" hide-the-main-window))
                 )))
    
    (show-window *demoWindow*) 
    
    (ask-for-params 
     theParamsQueryField 
     param-query-string 
     1 max-iterations)))



(define (dialog-gui generate-answer welcome)
  ; generate-answer: string -> string
  ;  welcome: string
  
  ;generate-answer is a procedure, that takes a string
  ; and generates an answer-string.
  ; welcome is z´he initial question.
  (let* ((theQuery (make-message welcome))
         (theAnswerQueryField (make-text (make-string 80 #\SPACE)))
         (read-answer 
          (lambda (event) 
            (let*
                ((the-question-entered 
                  (text-contents theAnswerQueryField)))
              (draw-message 
               theAnswerQueryField
               (generate-answer the-question-entered))))))
    
    (set! *demoWindow* 
          (create-window
           (list (list theQuery)
                 (list theAnswerQueryField )
                 (list    
                  (make-button "QUIT" hide-the-main-window)
                  (make-button "o.k." read-answer )
                  ))))
    (show-window *demoWindow*)))

(define (PerformTheDemoSelected
         demos-available
         thechoices 
         event)
  (let* ((index (choice-index thechoices))
         (entry (list-ref demos-available index ))
         ; decode the selection index and call the procedure
         (action (the-action entry)))
    (action)
    #t))

(define (demo-without-params demos-available)
  (let* ((theDemos (map the-query-string demos-available))
         (theQuery (make-message "Please select a demo:"))
         (theChoices (make-choice theDemos)))
    
    (set! *demoWindow* 
          (create-window
           (list (list theQuery
                       theChoices)
                 (list 
                  (make-button "o.k." 
                               (curry 
                                PerformTheDemoSelected
                                demos-available
                                theChoices)))
                 (list (make-button "QUIT" hide-the-main-window))
                 )))
    (show-window *demoWindow* )))

