#lang racket/gui

#|
################################################################################
##                                                                            ##  
##            This file is part of the se3-bib Racket module v3.0             ##  
##                Copyright by Leonie Dreschler-Fischer, 2010                 ##
##              Ported to Racket v6.2.1 by Benjamin Seppke, 2015              ##  
##                                                                            ##  
################################################################################
|#

(provide say canSpeak? 
           Victoria Agnes Kathy; women
           Ralph Bruce Fred Albert; men
           Princess Junior ; childen
           Deranged Bubbles Hysterical Whisper ; fancy
           Zarvox Boing  Trinoids ; Computer
           Bells Cellos Good-News Pipe-Organ; song
           beep
           shell launch-process)         
  
(require se3-bib/tools-module)

(define (shell command-string) 
  ; deprecated, zur Kompatibilitaet mit frueheren Versionen
  (system command-string))

(define (beep)
  (display "\a\a\a\a\n"); alert char
  (bell)) ; system bell from racket/gui

(define canSpeak? 
  (equal? (system-type) 'macosx))

(define (say text-to-say voice)
  ; String: text-to-say, String: voic‚e
  (let ((theCommand 
         (string-append
          "say -v \"" voice  "\" \"" text-to-say   "\""
          )))
    (if canSpeak? 
        (begin 
          (display theCommand)(newline)
          (system theCommand))
        (begin (display text-to-say)))))

;; voices, female adults
(define (Victoria thetext)
  (say thetext "Victoria"))
(define (Agnes  thetext)
  (say  thetext "Agnes"))
(define (Kathy  thetext)
  (say  thetext "Kathy"))
;; voices, male adults
(define (Ralph  thetext)
  (say  thetext "Ralph"))
(define (Bruce  thetext)
  (say  thetext "Bruce"))
(define (Fred  thetext)
  (say  thetext "Fred"))
(define (Albert  thetext)
  (say  thetext "Albert"))
;; voices, children
(define (Princess  thetext)            
  (say  thetext "Princess"))
(define (Junior  thetext)
  (say  thetext "Junior"))
;; voices, fancy
(define (Deranged  thetext)
  (say  thetext "Deranged"))
(define (Bubbles  thetext)
  (say  thetext "Bubbles"))
(define (Hysterical  thetext)
  (say  thetext "Hysterical"))
(define (Whisper  thetext)
  (say  thetext "Whisper"))
;; voices, computer
(define (Zarvox  thetext)
  (say  thetext "Zarvox"))
(define (Boing  thetext)
  (say  thetext "Boing"))
(define (Trinoids  thetext)
  (say  thetext "Trinoids"))
;; voices, song
(define (Bells thetext)
  (say  thetext "Bells"))
(define (Cellos thetext)
  (say  thetext "Cellos"))
(define (Good-News  thetext)
  (say  thetext "Good News"))
(define (Pipe-Organ  thetext)
  (say  thetext "Pipe Organ"))

(define (launch-process command arg)
  ; start a process connected by pipes
  (let* ((the-ports (process command arg))
         (pipe-standard-out-from-sp (list-ref the-ports 0))
         ;an input port piped from the subprocess's standard output,
         (pipe-standard-in-to-sp (list-ref the-ports 1))
         ;an output port piped to the subprocess standard input,
         (process-id-sp (list-ref the-ports 2))
         ; the system process id of the subprocess,
         (pipe-standard-err-sp (list-ref the-ports 3))
         ; an input port piped from the subprocess's standard error,10
         (status-sp (list-ref the-ports 4))
         (handler 
          (lambda (msg)
            (case msg
              ((stdin) pipe-standard-in-to-sp)
              ((stdout) pipe-standard-out-from-sp)
              ((id) process-id-sp)
              ((status-hnd) status-sp)
              ((status wait interrupt kill) (status-sp msg))
              #|
                 - 'status returns the status of the subprocess as one of 'running, 'done-ok, or 'done-error.
                 - 'wait blocks execution in the current thread until the subprocess has completed.
                 - 'interrupt sends the subprocess an interrupt signal under Unix and Mac OS X and takes no action under Windows. The result is void.
                 - 'kill terminates the subprocess and returns void.
              |#
              (else (error "unknown message"))))))
    handler))
