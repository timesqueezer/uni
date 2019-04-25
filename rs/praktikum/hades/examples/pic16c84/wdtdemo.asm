	TITLE           "WDT-Demo"
	SUBTITLE        "Benutzung des Watchdog-Timer"
;
;***********************************************************************
; In diesem Beispiel wird die Anzahl der aufeinander folgenden 
; WDT-Resets gezaehlt und auf Port B ausgegeben. Dabei wird die
; Zeitspanne bis zum WDT-Ueberlauf moeglichst kurz gewaehlt. 
;***********************************************************************
;
  Processor       16C84
  Radix           DEC
  EXPAND
;
  include         "p16c84.inc"
;
_ResetVector      set 0x00

Zaehler           set 0x10


  ORG     _ResetVector        ; Hier beginnt die Programmausfuehrung
  call    Init                ; Initialisierung

InfiniteLoop:
  goto    InfiniteLoop        ; Sinnlose Endlosschleife

;***********************************************************************
; Die Initialisierung prueft, ob ein WDT-Reset aufgetreten ist. Wenn ja, 
; wird der Zaehler hochgezaehlt. Sonst wird der Zaehler zurueckgesetzt.
; Der Zaehlerstand wird an Port B ausgegeben.
;***********************************************************************

Init:
  movlw   0xff                ; 0xff ins Work-Register
  btfsc   STATUS, 4           ; Ist das Time-out-Bit gesetzt?
  movwf   Zaehler             ; Wenn nicht, dann 0xff in den Zaehler
  clrwdt                      ; Nun schnell WDT zuruecksetzen
  incf    Zaehler,F           ; Zaehler erhoehen, falls kein WDT-Reset
                              ; war, steht nun eine 0 im Zaehler
  bsf     STATUS, RP0         ; Bank 1 aktivieren
  movlw   b'11110111'         ; Prescaler an externen TMR0
  movwf   OPTION_REG          ; und ins OPTION-Register damit
  movlw   0x00                ; Maske fuer Ausgangswahl
  movwf   TRISB               ; ins TRISB
  bcf     STATUS, RP0         ; Wieder Bank 0
  movf    Zaehler,W           ; Zaehler ins Work-Register
  movwf   PORTB               ; und dann am Port B ausgeben
  return

  END
