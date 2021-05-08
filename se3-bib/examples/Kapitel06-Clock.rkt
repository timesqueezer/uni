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

(require  (except-in 2htdp/universe space)
          2htdp/image
          test-engine/racket-tests 
          racket/trace
          se3-bib/tools-module
          se3-bib/examples/images-module)

; the with of the canvas
(define *clock-canvas-w* 
  (image-width *clock-face*))

; height of canvas
(define *clock-canvas-h* 
  (image-height *clock-face*))

(define (next-clock-world world )
  ; next-clock-world: date? --> date?
  (seconds->date 
   (current-seconds)))

(define (place-hand frac-of-cycle len colo canvas)
  ; drwa a hand of the clock
  ; number: the fraction of the 12 hour cycle
  ; number: the length relative to the longest hand
  ; color: the color
  (let* ((y (* len 0.4 *clock-canvas-h*))
         (angRad (- (* frac-of-cycle 2 pi)))
         ;x'=x*cos(angle)-y*sin(angle)
         ;y'=y*cos(angle)+x*sin(angle)
         (xn (- (* y (sin angRad)))); rotated x at the origin
         (yn (* y (cos angRad)));rotated y at the origin
         (xm (/  *clock-canvas-w* 2))
         (ym (/  *clock-canvas-h* 2)))
    ; y-axis points down, not up!
    (add-line canvas xm ym (+ xn xm) (- ym yn ) colo)
    ))

(define (hour-hand hour minute canvas)
  ; real --> Scene
  (place-hand (/ (+ hour (/ minute 60))
                 12) 0.6 'blue canvas))

(define (minute-hand minutes seconds canvas)
  ; real --> Scene
  (place-hand (/ (+ minutes (/ seconds 60))
                 60) 1 'blue canvas))

(define (seconds-hand seconds canvas)
  ; real --> Scene
  (place-hand (/ seconds 60)
              1.0 'red canvas))

(define *zeitzonenStandard* ; Strings der Standardzeiten
  (vector 
   "GMT"; 0 , "UTC", "WET" Island, Marokko, Kanaren, Senegal ...
   "MEZ"; 1
   "EET"; 2 Türkei, Israel, Ägypten...
   "MSK"; 3 Moskau
   "SAMT"; 4 Samara
   "TMT"; 5 Turkmanistan
   "NOVT"; 6 Novosibirsk
   "WAST"; 7 West australian
   "CCT";8 China
   "JST"; 9 Japan
   "EAST"; 10 East Australian
   "MAG"; 11 Magadan
   "NZST"; 12 New Zealand
   "NT"; -11 Nome Time
   "HST"; -10 Hawaii, "CAT" CEntral Alaska
   "YST"; -9 Yukon Standard
   "PST"; -8 Pacific 
   "MST"; -7 Mountain 
   "CST";-6 Central:Canada, USA, Mexiko
   "EST"; -5 Canada, USA, Kolumbien, Peru
   "AST"; -4 Atlantik: :Canada,Venezuela, Brasilien, Paraguay, Chile, Bolivien
   "ART"; -3 Argentinien, Brasilien, Uruguay, Franz. Guayana
   "FNT"; -2 Fernando de Noronha
   "AT"; -1 Acores
   
   ))
(define *zeitzonenSommer* 
  (vector ; Strings fuer Sommerzeiten
   "AST"; -1 Acores                     
   "WEST"; 0 , "UTC", "WET" Island, Marokko, Kanaren, Senegal ...
   "MESZ"; 1 Deutschland, Frankreich
   "EEST"; 2 Türkei, Israel, Ägypten...
   "MSKST"; 3 Moskau
   "SAMST"; 4 Samara
   "TMST"; 5 Turkmanistan
   "NOVST"; 6 Novosibirsk
   "WASST"; 7 West australian
   "CCST";8 China
   "JSST"; 9 Japan
   "EASST"; 10 East Australian
   "MAGST"; 11 Magadan
   "NZSST"; 12 New Zealand
   "NST"; -11 Nome Time
   "HSST"; -10 Hawaii, "CAT" CEntral Alaska
   "YSST"; -9 Yukon Standard
   "PSST"; -8 Pacific 
   "MSST"; -7 Mountain 
   "CSST";-6 Central:Canada, USA, Mexiko
   "ESST"; -5 Canada, USA, Kolumbien, Peru
   "ASST"; -4 Atlantik: :Canada,Venezuela, Brasilien, Paraguay, Chile, Bolivien
   "ARST"; -3 Argentinien, Brasilien, Uruguay, Franz. Guayana
   "FNST"; -2 Fernando de Noronha
   ))
(define (zonenkuerzel datum)
  (let* ([offset (/(date-time-zone-offset datum) 3600)]
         [sommerzeit? (date-dst? datum)]
         [tabpos (if (negative? offset) 
                     (+ offset 24) offset)])
    ; Zeitzonenoffset auf das Intervall 0..24 normieren
    (if (integer? tabpos)        
        (if sommerzeit? 
            (vector-ref *zeitzonenSommer* tabpos)
            (vector-ref *zeitzonenStandard* tabpos))
        (format "GMT+~a" offset))
    ))


(define (draw-clockworld world)
  ; draw-clockworld: date --> Image
  (let* ( [world (seconds->date 
                  (current-seconds))]
          [h (image-height *clock-face*)]
          [w (image-width *clock-face*)]
          [canvas 
           (empty-scene   
            *clock-canvas-w*
            *clock-canvas-h*)]
          [hour (date-hour world)]
          [min (date-minute world)]
          [sec (date-second world)] )
    (display (list 
              hour ":" min ":" sec
              (zonenkuerzel world)))
    (display        "\n")
    
    (hour-hand 
     hour min
     (minute-hand
      min sec
      (seconds-hand 
       sec 
       (place-image
        *clock-face* 
        (/ *clock-canvas-w* 2)
        (-(/ *clock-canvas-h* 2) 2)
        canvas))))
    ))

(define (animate-clock)
  (display "To stop the clock simply close the clock window.\n")
  (animate draw-clockworld))



; Variante 2:
; Universum mit action handlern fuer Interaktion

; Der Datentyp fuer den Weltzustand ist das aktuelle Datum.

(define *last-clock-world* 
  (seconds->date 2000000000))
; return a special date to signal the end of time

(define (clock-stopped? world)
  (equal? world *last-clock-world*))

(define (handle-key world key)
  ; Abbruch der Simulation mit "q"
  (if (or (string=? key "escape")
          (string=? key "q"))
      (begin (display "escape or q\n")
             (stop-with world);
             )
      world)
  ) 

(define (sim-clock tick-rate)
  ; sim-cloc: positive-real --> true 
  ; tick: time between clock ticks (seconds)
  (let ((first-clock 
         (next-clock-world #t)))
    (display "Type 'q' or escape to stop the clock.\n")
    (big-bang 
     first-clock
     (on-tick 
      next-clock-world tick-rate)
     (to-draw 
      draw-clockworld
      *clock-canvas-w*
      *clock-canvas-h*)
     (stop-when clock-stopped?)
     (on-key handle-key)
     (record? #t)
     (name "Clock"))))



;Einfache Animation testen mit (animate-clock)
; Interaktive Animation testen mit: (sim-clock)

(provide animate-clock 
         sim-clock)