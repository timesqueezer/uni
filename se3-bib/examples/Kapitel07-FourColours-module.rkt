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

(provide color-graph
         four-colors-demo
         *map1*
         demo1  demo2); demo3 demo4

(require
  2htdp/image
  (except-in 2htdp/universe space)
  scheme/list
  racket/trace
  se3-bib/tools-module
  se3-bib/vector-graphics-module
  se3-bib/backtracking-module
  se3-bib/macos-module
  se3-bib/grid-map-module)

;;; ==========================================================================
;;; color a map using only a given number of colors
;;; using the general backtracking tool.
;;; ==========================================================================

(define *moves-counter* 0); count the backtracking steps
(define *visual-limit* 20); after this limit of steps is reached turn animation of
; the pixels of the map
(define *map1* 
  (vector
   "AAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBB"
   "AAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBBBBFF"
   "AAAAAAAAAAAAAAAAAAAAAADDDBBBBBBBBBBBBBBBBFF"
   "AAAAAAAAAAAAAAAAAAAAAADDDBBBBBBBBBBBBBBBBFF"
   "AAAAAAAAAAAAAAAAAAAAAADDDBBBBBBBBBBBBBBBBFF"
   "AAAAAAAAAAAAAAAAAAADDDDDDBBBBBBCCCCCCCCCCFF"
   "AAAAAAAAAAAAAAAAADDDDDDDDCCCBBBBBBCCCCCCCFF"
   "AAAAAAAAAAAAAAAAAADDDDDDDCCCCCCCCCCCCCCCCFF"
   "AAAAAAAAAAAAAAAAAAADDDDDDCCCCCCCCCCCCCCCCFF"
   "AAAAAAAAAAAAAAAAAAADDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAAAADDDDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAADDDDDDDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAAADDDDDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAADDDDDDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAADDDDDDDDCCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAADDDDDDDDDDDDDCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAADDDDDDDDDDDCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAAAAAFFFFFFFCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAAAAAAAAAFFFCCCCCCCCCCCCCCCCFFF"
   "AAAAAAAAAAAAAFFFFFFFFFFCCCCCCCCCCCCCCCCCFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFCCCCCCCCCCCCCCCCCFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCCCCCCCCCFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCCCCFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
   ))

(define *nns-map1* ; the neighbor relation
  '((#\A #\D #\F)
    (#\B #\A #\C #\D #\F)
    (#\C #\B  #\D #\F)
    (#\D #\A #\B #\C #\F)
    (#\F #\A #\B #\C #\D)))
;;; Eine Nachbarschaftsrelation : Beispiel
(define *Nachbarschaftstabelle-Deutschland*
  ;; eine Assoziationsliste (( Land1 Nachbar11 Nachbar12 .. Nachbar1n) ...
  ;;                         ( Landn Nachbarn1 Nachbarn2 .. Nachbarnn))
  '((Hamburg Schleswig-Holstein Niedersachsen)
    (Schleswig-Holstein Hamburg Niedersachsen Mecklenburg-Vorpommern)
    (Niedersachsen Hamburg Bremen Hessen Brandenburg)
    (Bremen Niedersachsen)
    (Brandenburg Mecklenburg-Vorpommern Berlin Hessen)
    (Berlin Brandenburg)))

(define (Laender NTabelle) 
  ; eine Liste aller Laender
  (map car NTabelle))

(define (Nachbarn-von Land NTabelle) 
  (cdr (assoc Land NTabelle)))

(define *Farben* '(red green blue yellow 'black))
(define farblos #f)

;;; Repraesentation des Zustands der Suche: 
;;; -- Liste der gefaerbten Laender (Farbtabelle als Assoziationsliste)
;;; -- Liste der ungefaerbten Laender

(define (neue-Farbtabelle) '()) ; noch kein Land ist gefaerbt

(define (faerbe-Land Tabelle Land Farbe)
  (cons (cons Land Farbe) Tabelle))

(define (Farbe-von Land Tabelle) 
  (let ((Eintrag (assoc Land Tabelle)))
    (if Eintrag (cdr Eintrag) farblos)))

(define (aktuelles-Land Tabelle)
  (car (car Tabelle)))

(define (neuer-Zustand gefaerbte-Laender ungefaerbte-Laender)
  (list gefaerbte-Laender ungefaerbte-Laender))

(define (gefaerbte-Laender Zustand) (car Zustand))

(define (ungefaerbte-Laender Zustand) (cadr Zustand))

(define (Folgezustaende Zustand Farbpalette)
  (let 
      ((gefaerbt (gefaerbte-Laender Zustand))
       (ungefaerbt (ungefaerbte-Laender Zustand)))
    (if (null? ungefaerbt) '()
        (let* ((naechstes-Land (car ungefaerbt))
               (verlaengerte-Farbtabellen 
                (map (curry faerbe-Land gefaerbt naechstes-Land) 
                     Farbpalette))
               (neue-Zustaende
                (map (curryr neuer-Zustand (cdr ungefaerbt))
                     verlaengerte-Farbtabellen)))
          neue-Zustaende))))

(define (zulaessig?  Zustand Ntabelle)
  (let ((Farbtabelle (gefaerbte-Laender Zustand)))
    (if (null? Farbtabelle) #t
        (let* 
            ((Land (aktuelles-Land Farbtabelle))
             (Nachbarlaender (Nachbarn-von Land Ntabelle))
             (diese-Farbe (Farbe-von Land Farbtabelle)))
          (not (some (curry equal? diese-Farbe)
                     (map (curryr Farbe-von Farbtabelle)
                          Nachbarlaender)))))))

(define (alles-gefaerbt? Zustand)
  (null? (ungefaerbte-Laender Zustand)))

(define (color-graph 
         Nachbarschaftstabelle
         Farbpalette)
  (let* ((Ausgangszustand
          (neuer-Zustand (neue-Farbtabelle) 
                         (Laender Nachbarschaftstabelle)))
         (Loesungen 
          (general-backtracking-non-cyclic
           Ausgangszustand
           (curryr Folgezustaende Farbpalette)
           (curryr zulaessig? Nachbarschaftstabelle)
           alles-gefaerbt?
           display
           id
           )))
    (writeln "Es gibt " (length Loesungen)".")
    (unless (null? Loesungen)
      (writeln "Loesung 1: " (car Loesungen)))))

(define (color-graph-visually 
         Nachbarschaftstabelle
         Farbpalette
         show-state
         clear-state
         show-the-solution-if-successfull?)
  (let* ((Ausgangszustand
          (neuer-Zustand (neue-Farbtabelle) 
                         (Laender Nachbarschaftstabelle)))
         (Loesungen 
          (general-backtracking-non-cyclic
           Ausgangszustand
           (curryr Folgezustaende Farbpalette)
           (curryr zulaessig? Nachbarschaftstabelle)
           show-the-solution-if-successfull?
           show-state
           clear-state
           )))
    (writeln "Es gibt " (length Loesungen)".")
    (unless (null? Loesungen)
      (writeln "Loesung 1: " (car Loesungen)))
    Loesungen))

(define (current-display-color-map state)
  (let* ((current-color-settings (gefaerbte-Laender state))
         (the-countries (map car current-color-settings))
         (display-color-map 
          (make-color-map 
           the-countries 
           (map (curryr Farbe-von current-color-settings) 
                the-countries))))
    (set-color-map display-color-map)
    display-color-map))

(define (show-state state display-graph slowly)
  (set! *moves-counter* (add1 *moves-counter*))
  (draw-graph display-graph
              (current-display-color-map state)) 
  (when (and slowly (< *moves-counter* *visual-limit*))
    (sleep 1))
  state)

(define (clear-state state display-graph)
  (let ((the-countries (gefaerbte-Laender state)))
    (unless (null? the-countries)
      (let ((the-node-to-color 
             (aktuelles-Land (gefaerbte-Laender state))))
        (big-marker-node the-node-to-color 'white display-graph))))
  ;the current state overwrites old displays
  state)

(define (show-the-solution-if-successfull? state the-graph)
  (if (alles-gefaerbt?  state)
      (let ((cm (current-display-color-map state)))
        (paint-map)
        ;(draw-contour-map)
        (draw-graph the-graph cm)
        (when (< *moves-counter* *visual-limit*)
          (sleep 2))
        state) #f))

(define (four-colors-demo the-map n-colors)
  (init-drawing 
   the-map ; the pixmap image to be drawn
   *bw-color-map* ; the color table to use
   'black ; the drawing color for additional lines 
   'white)
  (set! *moves-counter* 0)
  (let* ((countries (region-list))
         (neighbors (neighbor-list countries))
         (the-graph (pixmap->connectivity-graph the-map)))
    ;(paint-map)
    (draw-contour-map)
    (draw-graph the-graph *bw-color-map*)
    (color-graph-visually  
     neighbors
     (take *Farben* n-colors)
     (curryr show-state the-graph (< n-colors 5))
     (curryr clear-state the-graph)
     (curryr show-the-solution-if-successfull? the-graph)
     )))

(define (demo1) 
  (four-colors-demo *map1* 3))

(define (demo2) 
  (four-colors-demo *map1* 4))


