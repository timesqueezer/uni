	TITLE           "16-Bit-Zaehler: PIC16C84"
	SUBTITLE        "Zaehlimpulse von RB0/INT"

;***********************************************************************
; Dieser Zaehler bekommt seine Zaehlimpulse von Port B/ Pin 0
; und gibt das Ergebnis an den oberen 4 Bits von Port B aus. Dieses 
; Beispiel ist fuer den PIC16C84 implementiert, sollte aber auch mit 
; anderen aehnlichen Bausteinen funktionieren. 
; Fuer die Ausfuehrung dieses Beispiels muss der WDT deaktiviert sein.
;***********************************************************************

		Processor       16C84
		Radix   DEC
		EXPAND

		include         "p16c84.inc"

_ResetVector	set	0x00
_IntVector	set	0x04

Zaehler 	set 	0x20


	ORG     _ResetVector	; Hier beginnt die Programmausfuehrung
	call    Init		; Initialisierung

InfiniteLoop:
	goto    InfiniteLoop	; Sinnlose Endlosschleife

	ORG     _IntVector	; Interrupt-Adresse
	goto    Interrupt	; Auswertung des Interrupts

;***********************************************************************
; Die Initialisierung setzt die unteren 4 Bits von PortB als Eingang
; und die oberen vier als Ausgang. Ausserdem wird der Interrupt fuer
; RB0/INT erlaubt und der Zaehler zurueckgesetzt
;***********************************************************************

Init:
	bsf	STATUS, RP0	; Bank 1 aktivieren
	movlw	0xC0		; Steigende Flanke, kein Pullup fuer
	movwf	OPTION		; RB0/INT-PIN festlegen
	movlw	0x10		; Nur RB0/INT erlauben
	movwf	INTCON		; INTCON ist in beiden Baenken
	movlw	0x0F		; Maske fuer Eingangswahl
	movwf   TRISB		; ins TRISB
	bcf	STATUS, RP0	; Wieder Bank 0
	clrf	Zaehler		; Der Zaehler wird genullt
	retfie			; RETURN mit Interrupt aktivieren

;***********************************************************************
; Die Interrupt-Service-Routine ist in diesem Falle einfach, da sich
; nicht um die Umgebung gekuemmert werden muss, also keine Status-Bits
; und kein Work-Register gespeichert und wieder hergestellt werden muss.
; Hier wird einfach gezaehlt und das Ergebnis an Port B angelegt.
;***********************************************************************

Interrupt:
	incf	Zaehler		; Zaehler erhoehen
	swapf	Zaehler,W	; Zaehler-Nibbles vertauscht ins W
	movwf	PORTB		; Und in Port B schreiben
	bsf	STATUS, RP0	; Bank 1 aktivieren
	movlw	0x10		; Nur RB0/INT erlauben
	movwf	INTCON		; Ab ins INTCON damit
	bcf	STATUS, RP0	; Bank 0 aktivieren
	retfie			; Interrupt verlassen

