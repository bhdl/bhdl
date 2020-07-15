#lang racket
(require bhdl
         (prefix-in pict: pict))

; Example from https://github.com/lihebi/bhdl/blob/master/docs/assets/fuse-sch.png

;; the IC type
;; FIXME assuming DIP and some random pin order here
(define/IC (JW5211)
  #:DIP (8 VIN EN GND FB SW GND GND GND))

(define (circuit-fn Rv_value Rh_value C_value L_value)
  (make-Composite 
   #:external-pins (FUSE_PWR Vout GND)
   #:vars ([Rv (R Rv_value)]            ; vertical resistor, R1, R2, or R3
           [Rh (R Rh_value)]            ; vertical resistor, R4, R5, or R6
           [C (C C_value)] ; first C is a variable, second C is a part name from library
           ;; FIXME using R as there's no L library yet
           [L (R L_value)] ; first L is a variable, second L is a part name from library
           [U (make-IC-atom JW5211)]    ; the chip
           )
   ;; #:connect (*- C.1 self.GND)
   #:connect (*+ ((U.VIN U.EN self.FUSE_PWR)
                     (U.GND self.GND Rv.2 C.2)
                     (U.FB Rv.1 Rh.1)
                     (C.1 Rh.2 L.2 self.Vout)
                     (U.SW L.1)))
   ;; CAUTION for now, to place and visualize a circuit, it must has a layout,
   ;; use a 300x200 rectangle as default canvas
   #:layout (pict:rectangle 300 200)         ; let's ignore layout for now. 
   ))

;; call the function to create circuit
(define circuit (circuit-fn 1 2 3 4))
;; initially placed on center
(define init-place (Composite->place-spec circuit))
;; visualize (this will show picture in REPL)
(Composite->pict circuit init-place)

;; generate KiCAD
(define kicad-str (Composite->kicad-pcb circuit init-place))
(call-with-output-file "/tmp/fuse.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write kicad-str out)))
