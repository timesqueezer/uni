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

(require se3-bib/prolog/prologInScheme)

;(ausleihe Signatur Lesernummer)
(<- (ausleihe "K 110" 100))
(<- (ausleihe "P 30" 102))
(<- (ausleihe "P 32" 104))
(<- (ausleihe "P 50" 104))
;( vorbestellung Signatur Lesernummer)
(<- (vorbestellung "K 110" 104))
(<- (vorbestellung "K 110" 102))
(<- (vorbestellung "P 30" 100))
(<- (vorbestellung "P 30" 104))
; (leser Name Vorname Lesernummer Geburtsjahr)
(<- (leser Neugierig Nena 100 1989))
(<- (leser Linux Leo 102 1990))
(<- (leser Luator Eva 104 1988))

; (?- (ausleihe "K 110" ?Leser))
; (?- (leser Linux Leo ?LN ?-))
; (?- (ausleihe "P 30" ?LN)(leser  ?Nam ?Vnam ?LN ?))
; (?- (ausleihe ? ?LN)(leser  ?Nam ?Vnam ?LN ?Jahr) (test (> (- 2008 60) ?Jahr)))
; (?- (leser ?name ?vorname ?nummer ?)
;    (ausleihe ?buch1 ?nummer)
;    (ausleihe ?buch2 ?nummer) ;    (!= ?buch1 ?buch2)
;    (test (string<? ?buch1 ?buch2)))
