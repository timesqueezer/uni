#lang racket

; 6549385 Lorenzo Luciano
; 6930992 Franz Krekeler
; 6946325 Matz Radloff

; Aufgabe 1.1

(define
  (deg->rad deg)
  (* pi
    (/ deg 180)
  )
 )

(define
  (rad->deg rad)
  (* rad
    (/ 180 pi)
  )
)

; Aufgabe 1.2

(define
  (my-acos c)
  (atan
    (/
      (sqrt
        (- 1
          (expt c 2)
        )
      )
    ) c
  )
)

; Aufgabe 1.3

(define
  (nm->km nm)
   (* nm 1.852)
)

; Aufgabe 2.1

(define
  (distanceAB latA longA latB longB)
  (nm->km
    (* 60
      (rad->deg
        (my-acos
          (+
            (*
              (sin (deg->rad latA))
              (sin (deg->rad latB))
            ) ; *
            (*
              (cos (deg->rad latA))
              (cos (deg->rad latB))
              (cos (deg->rad
                (- longB longA)
              ))
            ) ; *
          ) ; +
        ) ; my-acos
      ) ; rad-deg
    ) ; *
  ) ; nm->km
)

(distanceAB 59.93 10.75 22.2 114.1) ; Oslo -> Hongkong
(distanceAB 37.75 122.45 21.32 157.83) ; San Francisco -> Honululu
(distanceAB 27.1 109.4 12.1 77.05) ; Osterinsel -> Lima

; Aufgabe 2.3

(define
  (Grad->Himmelsrichtung deg)
  (cond
    [(and (> deg  348.75) (<= deg  11.25)) "N"]
    [(and (> deg  11.25) (<=  deg 33.75)) "NNE"]
    [(and (> deg  33.75) (<=  deg 56.25)) "NE"]
    [(and (> deg  56.25) (<=  deg 78.75)) "ENE"]
    [(and (> deg  78.75) (<=  deg 101.25)) "E"]
    [(and (> deg  101.75) (<= deg  123.75)) "ESE"]
    [(and (> deg  123.75) (<= deg  146.25)) "SE"]
    [(and (> deg  146.25) (<= deg  168.75)) "SSE"]
    [(and (> deg  168.75) (<= deg  191.25)) "S"]
    [(and (> deg  191.25) (<= deg  213.75)) "SSW"]
    [(and (> deg  213.75) (<= deg  236.25)) "SW"]
    [(and (> deg  236.25) (<= deg  258.75)) "WSW"]
    [(and (> deg  258.75) (<= deg  281.25)) "W"]
    [(and (> deg  281.25) (<= deg  303.75)) "WNW"]
    [(and (> deg  303.75) (<= deg  326.25)) "NW"]
    [(and (> deg  326.25) (<= deg  348.75)) "NNW"]
  )
)

(Grad->Himmelsrichtung 250)
(Grad->Himmelsrichtung 90)
