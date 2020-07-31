;; #lang racket
;; #lang s-exp "bhdl/splicing.rkt"
;; #lang bhdl
#lang s-exp bhdl/splicing

(require
 ;; require bhdl for production use
 ;; bhdl
 ;; require the file for incremental development
 "../bhdl-lib/bhdl/main.rkt"
         
 (prefix-in pict: pict)
 json
 (for-syntax syntax/parse
             racket/string))

(define global
  (make-circuit
   #:external-pins (GND 3V3 5V USB5V)))

(define (key-name-filter name)
  (if (string-prefix? name "k")
      (substring name 1)
      name))

;; or just key-with-diode group
(define (create-switch-fn spacing)
  (lambda (name)
    (if (= spacing 0)
        #f
        (let* ([atom (Cherry spacing)]
               [pict-with-name (cc-superimpose (atom->fp-pict atom)
                                               (pict:text
                                                (key-name-filter
                                                 (symbol->string name))))]
               ;; FIXME more functional way to do this?
               [atom (begin (set-Atom-pict! atom pict-with-name)
                            atom)]
               ;; key with diode group
               [key-with-diode (make-circuit
                                ;; FIXME user should not specify left and right
                                #:external-pins (left right)
                                #:vars ([d (1N4148W)])
                                #:connect (*- self.left atom d self.right)
                                #:layout (vc-append 3 atom d))])
          key-with-diode))))

(struct KeyboardMatrix
  (rows))

(define (keyboard-row mat i)
  (filter identity (list-ref (KeyboardMatrix-rows mat) i)))
(define (keyboard-col mat i)
  (filter identity
          (for/list ([row (KeyboardMatrix-rows mat)])
            (list-ref row i))))
(define (keyboard-xy mat x y)
  (list-ref (list-ref (KeyboardMatrix-rows mat) x) y))

(begin-for-syntax
 (define-syntax-class key-spec
   #:datum-literals (-)
   (pattern - #:with name (car (generate-temporaries '(a)))
            #:with spacing #'0)
   (pattern ID:id #:with name #'ID
            #:with spacing #'1)
   (pattern (ID:id spacing0:number)
            #:with name #'ID
            #:with spacing #'spacing0)))

(define-syntax (define-key-matrix stx)
  (syntax-parse
   stx
   [(_ name ([key:key-spec ...] ...))
    #'(begin
        ;; 1. define the atoms
        (define key.name ((create-switch-fn key.spacing) 'key.name))
        ... ...
        ;; 2. assign matrix to the variable
        (define name (KeyboardMatrix (list (list key.name ...) ...))))]))

(define-key-matrix matrix
  ([(esc 1.5)    k1 k2 k3 k4    k5       -         -        k6 k7 k8  k9  k0   (backspace 1.5)]
   [(tab 1.5)    Q  W  E  kR     T        -         -        Y  U  I   O   P    (k\\ 1.5)]
   [(caps 1.5)   A  S  D  F     G        (lmid1 1.5) (rmid1 1.5)        H  J  K   L   k\;  (enter 1.5)]
   [(lshift 1.5) Z  X  kC  V     B        (lmid2 1.5) (rmid2 1.5)        N  M  k\, k\. k\/  (rshift 1.5)]
   [lctrl lfn lsuper lalt lTBD (lspace 2.75) - - (rspace 2.75) rTBD ralt rsuper rfn rctrl]))

(module+ test
  (KeyboardMatrix-rows matrix)
  (keyboard-row matrix 1)
  (keyboard-col matrix 6))

(define (make-half left-or-right col1 col2 col3 col4 col5 col6 col7)
  (let-values
      ([(vr-append vl-append hb-append pi)
        (case left-or-right
          [(left)  (values vr-append vl-append hb-append pi)]
          [(right) (values vl-append vr-append
                           (lambda (arg . args)
                             (hb-append (reverse (cons arg args)) ..))
                           (- pi))])])
    (let* ([padding (λ (unit)
                      (pict:ghost (pict:rectangle 10 unit)))])
      (rotate (hb-append (vr-append col1 ..)
                         (vr-append col2 .. (padding 30))
                         (vr-append col3 .. (padding 60))
                         (vr-append col4 .. (padding 80))
                         (vr-append col5 .. (padding 60))
                         ;; col6
                         (match (list col6 col7)
                           [(list (list k5 t g b lspace) (list lmid1 lmid2))
                            (vl-append (hb-append (vl-append  k5 t g b)
                                                  (vl-append
                                                   (rotate lmid1 (/ pi 2))
                                                   (rotate lmid2 (/ pi 2))))
                                       lspace
                                       (padding 30))]))
              (- (/ pi 10))))))

;; TODO I need to support two kinds of location.
;; 1. functional pict
;; 2. specify absolute location directly
;; 3. possibly mixing the two?

(define matrix-module
  (make-circuit
   ;; FIXME these should be row[5] col[14], or get from matrix
   #:external-pins (row1 row2 row3 row4 row5
                         col1 col2 col3 col4 col5 col6 col7
                         col8 col9 col10 col11 col12 col13 col14)
   #:layout (inset (hc-append -150
                              (make-half 'left
                                         (for/list ([i (range 7)])
                                           (keyboard-col matrix i))
                                         ..)
                              (make-half 'right
                                         (for/list ([i (reverse (range 7 14))])
                                           (keyboard-col matrix i))
                                         ..))
                   -30)
   ;; connections
   #:connect (for/list ([x (range 5)])
               (filter-not
                void?
                (for/list ([y (range 14)])
                  (let ([key (keyboard-xy matrix x y)]
                        ;; FIXME this is ugly
                        [xname (string->symbol (~a "row" (add1 x)))]
                        [yname (string->symbol (~a "col" (add1 y)))])
                    (when key
                      (*-
                       (pin-ref self yname)
                       key
                       (pin-ref self xname)
                       ))))))))

(module+ debug
  ;; FIXME why no atoms?
  (collect-all-atoms matrix-module)

  (Composite->netlist matrix-module)
  (parameterize ([current-directory "/tmp/bhdl/"])
    (circuit-export matrix-module
                    #:auto-place #f #:formats '(kicad pdf dsn)))
  (parameterize ([current-directory "/tmp/bhdl/"])
    (circuit-export (make-circuit #:layout matrix-module
                                  ;; FIXME why this diode is not showing up?
                                  #:connect (*- matrix-module.row1
                                                (R)
                                                matrix-module.col1))
                    #:auto-place #f #:formats '(kicad pdf dsn)))

  (define c (make-circuit
             #:external-pins (p1 p2 p3)
             #:connect (*- self.p1 self.p3 (1N4148W) (1N4148W) self.p2)))

  (collect-all-atoms c)
  (collect-all-atoms (*- c))

  (nplaced-atoms matrix-module)
  (nfree-atoms matrix-module)
  (collect-all-composites matrix-module))

(define gd32-module
  (make-circuit
   ;; TODO signals to use in this circuit
   ;; #:signal ([GND 3V3])
   #:external-pins (SPI_CS SPI_CLK SPI_MISO SPI_MOSI
                           ;; TODO how to abstract over pin names
                           row1 row2 row3 row4 row5
                           col1 col2 col3 col4 col5 col6 col7
                           col8 col9 col10 col11 col12 col13 col14)
   ;; variables
   #:vars ([gd32 (GD32VF103CBT6)]
           [x2 (Crystal-2)]
           [x4 (Crystal-4)]
           [reg (ME6211C)]
           [usb (USB-C-16)]
           ;; [rgb (TJ-S1615CY)]
           ;; TODO I probably want to use 3 leds in a row?
           ;; TODO I should also have a poweron LED?
           [rgb (WS2812B)]
           [btn-boot (SKRPACE010)]
           [btn-reset (SKRPACE010)])
   ;; setup external pins
   #:connect (*= (self [row1 row2 row3 row4 row5])
                 (gd32 [PA0 PA1 PA2 PA3 PA4]))
   #:connect (*= (self [col1 col2 col3 col4 col5 col6 col7
                             col8 col9 col10 col11 col12 col13 col14])
                 (gd32 [PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7 PB8 PB9 PB10 PB11
                            PA5 PA6]))
   #:connect (*= (self [SPI_CS SPI_CLK SPI_MISO SPI_MOSI])
                 (gd32 [SPI1_CS SPI1_SCLK SPI1_MISO SPI1_MOSI]))
   ;; layout
   #:layout (vc-append (rotate usb pi) (hc-append btn-boot btn-reset rgb) gd32)
   #:connect (list
              ;; reset pin
              (*- gd32.NRST (*< (*- (C '100nf) global.GND)
                                (*- (R '10k) global.3V3)))
              ;; power to GD32
              ;;
              ;; FIXME necessary to connect all VDDs?
              (*+ ([global.3V3 gd32.VDD1 gd32.VDD2 gd32.VDD3]))
              (*+ ([global.GND gd32.VSS1 gd32.VSS2 gd32.VSS3]))

              ;; filtering ferrite-bead, analog VDDA and VSSA
              (*- gd32.VDDA (FerriteBead) global.3V3)
              (*- gd32.VSSA (FerriteBead) global.GND)
              (*- gd32.VDDA (C '100nf) gd32.VSSA)

              ;; filtering capacitors
              (*- global.3V3 (C '100nf) global.GND)
              (*- global.3V3 (C '100nf) global.GND)
              (*- global.3V3 (C '100nf) global.GND)
              (*- global.3V3 (C '100nf) global.GND)

              ;; 2pin crystal
              (*- global.GND (C '6pf) x2 (C '6pf) global.GND)
              ;; 4pin crystal
              (*- gd32.OSCIN x4.XIN)
              (*- gd32.OSCOUT x4.XOUT)
              (*- x4.XIN (C '6pf) x4.GND)
              (*- x4.XIN (C '6pf) x4.GND)
              (*- x4.GND global.GND)

              ;; voltage regulator
              ;;
              ;; FIXME place the capacitors close to where they are defined
              (*- reg.VIN global.5V (C '10uf) global.GND)
              (*- reg.VIN (LED0603 'red))
              (*- reg.VSS global.GND)
              (*- reg.CE global.5V)
              (*- reg.VOUT global.3V3 (*< (C '100uf)
                                          (C '100nf))
                  global.GND)
            
              ;; USB type-c
              (*- usb.GND global.GND)
              ;; connect usb power to 5V
              (*- usb.VBUS (Fuse) global.5V)
              (*- (*< usb.CC1 usb.CC2) (R '5.1k) global.GND)
              (*- (*< usb.D+1 usb.D+2) (R 22) gd32.USBD+)
              (*- (*< usb.D-1 usb.D-2) (R 22) gd32.USBD-)
              ;; FIXME connect the other 4 pins of usb (pads) to GND?
            
              ;; FIXME NC?
              ;; (*- gd32.VBAT (R 'NC) global.3V3)

              ;; LED TODO placement
              ;; (*- global.3V3 (R '2k) (LED 'red) global.GND)
              ;; (*- global.3V3 rgb.VIN)
              ;; FIXME the GPIO to use
              ;; (*- rgb.R (R '4k7) gd32.PC13)
              ;; (*- rgb.G (R '4k7) gd32.PA1)
              ;; (*- rgb.B (R '4k7) gd32.PA2)
              ;;
              ;; UPDATE using the ws2812
              (*- rgb.VDD global.5V (C '100nf) rgb.VSS global.GND)
              (*- rgb.DI gd32.PC13)

              ;; boot button
              (*- global.3V3 btn-boot gd32.BOOT0 (R '10k) global.GND)
              (*- gd32.BOOT1 (R '10k) global.GND)
              ;; reset button
              (*- gd32.NRST btn-reset global.GND))))


(define esp32-module
  (make-circuit
   ;; FIXME I intend this as internal signal name
   #:external-pins (EXT5V
                    SPI_CS SPI_CLK SPI_MISO SPI_MOSI)
   #:connect (*= (self [SPI_CS SPI_CLK SPI_MISO SPI_MOSI])
                 (esp32 [VSPICS0 VSPICLK VSPIMISO VSPIMOSI]))
   #:layout (ht-append (inset esp32 0 -20 0 0)
                       (vc-append (rotate usb pi)
                                  (hc-append rgb btn-io0 btn-en)))
   #:vars ([esp32 (ESP32-WROVER-E)]
           [usb (USB-C-16)]
           [cp2102n (CP2102N)]
           ;; FIXME BJT and pin name and order
           [q1 (SS8050-G)]
           [q2 (SS8050-G)]
           [reg (AMS1117-3.3)]
           [rgb (WS2812B)]
           ;; TODO what are the purpose of these buttons? Reset? Boot?
           [btn-io0 (Switch)]
           [btn-en (Switch)])
   #:connect (list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; usb connector
              (*- usb.VBUS (R '22.1k) cp2102n.VBUS)
              (*- usb.VBUS
                  ;; FIXME polarized and /-\
                  (1N4148W) self.EXT5V)
              (*- usb.GND global.GND)
              ;; data bus
              ;; FIXME diode: LESD5D5
              (*- usb.D+1 cp2102n.D+ (1N4148W) global.GND)
              (*- usb.D-1 cp2102n.D- (1N4148W) global.GND)
              ;; cp2102n additional periphral
              ;; FIXME DCD RI??
              (*- cp2102n.VBUS (R '47.5k) global.GND)
              (*- cp2102n.RSTb (R '2k) reg.VOUT)
              (*- cp2102n.SUSPEND (R '10k) global.GND)
              (*- cp2102n.GND global.GND)
              (*- cp2102n.VDD cp2102n.VREGIN reg.VOUT
                  (*< (C '4.7uf)
                      (C '0.1uf))
                  global.GND)
              (*- cp2102n.GND global.GND)


              ;; RGB
              (*- rgb.VDD global.5V (C '100nf) rgb.VSS global.GND)
              (*- rgb.DI esp32.IO2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; connect cp2102 to esp32
              ;; FIXME DSR CTS?
              ;; the auto program protection circuit
              (*- cp2102n.DTR (R '10k) q1.B)
              (*- cp2102n.RTS (R '10k) q2.B)
              (*- cp2102n.DTR q2.C)
              (*- cp2102n.RTS q1.E)
              ;; connect to esp32
              (*- q1.C esp32.EN)
              (*- q2.E esp32.IO0)
              ;; DTR RTS EN IO0
              ;; 1   1   1  1
              ;; 0   0   1  1
              ;; 1   0   0  1
              ;; 0   1   1  0
              (*- cp2102n.TXD (R 0) esp32.RXD0)
              (*- cp2102n.RXD (R 0) esp32.TXD0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; power supply
              (*- self.EXT5V (R '2k) (LED0603) global.GND)
              (*- self.EXT5V (C '22uf) global.GND)
              (*- self.EXT5V reg.VIN)
              (*- reg.GND global.GND)
              (*- reg.VOUT (C '22uf) global.GND)

              ;; button
              (*- esp32.IO0 (*< btn-io0
                                (C '0.1uf))
                  global.GND)
              (*- esp32.EN (*< btn-en
                               (C '0.1uf))
                  global.GND)

              ;; ESP32 module
              (*- reg.VOUT (*< (C '22uf)
                               (C '0.1uf)) esp32.3V3)
              (*- reg.VOUT (R '10k) esp32.EN (C '0.1uf) global.GND)
              (*- esp32.GND esp32.GND global.GND)

              ;; TODO pin header
              ;; TODO use vectorized connection for this in-place-created pin header
              ;; (*= (connector 19)
              ;;     (esp32 [EN NC NC IO34 IO35 IO23 IO33 IO25 IO25]))
              )))



;; TODO the rest of circuit
(define fitboard
  (make-circuit
   ;; CAUTION just to declare the pict
   #:layout (ct-superimpose (hc-append matrix-module)
                            (ht-append (pict:ghost (pict:rectangle 50 10))
                                       (ht-append 100 gd32-module esp32-module)))
   #:connect (list matrix-module esp32-module gd32-module
                   ;; connect matrix module with gd32 module
                   ;; TODO *= connects corresponding pins with same name by default
                   (*= (matrix-module [row1 row2 row3 row4 row5])
                       (gd32-module [row1 row2 row3 row4 row5]))
                   (*= (matrix-module [col1 col2 col3 col4 col5 col6 col7
                                            col8 col9 col10 col11 col12 col13 col14])
                       (gd32-module [col1 col2 col3 col4 col5 col6 col7
                                          col8 col9 col10 col11 col12 col13 col14]))
                   ;; connect esp32 and gd32 via SPI
                   (*= (gd32-module [SPI_CS SPI_CLK SPI_MISO SPI_MOSI])
                       (esp32-module [SPI_CS SPI_CLK SPI_MISO SPI_MOSI])))))

;; visualize the board
(Composite-pict fitboard)

;; visualizing init placement
(module+ test
  (make-directory* "/tmp/bhdl/")
  (parameterize ([current-directory "/tmp/bhdl/"])
    ;; TODO NOW HEBI enable auto-place
    (circuit-export fitboard
                    #:auto-place #t
                    #:use-cache #f
                    #:formats '(kicad pdf dsn ses)))

  ;; (parameterize ([current-directory "/tmp/bhdl/"])
  ;;   (circuit-export fitboard #:auto-place #t #:formats '(kicad pdf dsn ses)))
  (void))

(module+ debug
  (IC-name (ICAtom-ic (1N4148W)))
  (map Atom-pict (collect-all-atoms fitboard))
  (define place-spec (Composite->place-spec fitboard))
  (Composite->pict fitboard place-spec)
  ;; FIXME why the keys are not shown?
  (nplaced-atoms matrix-module)
  (nfree-atoms matrix-module)
  (nplaced-atoms fitboard)
  (nfree-atoms fitboard)
  (collect-all-atoms matrix-module)
  (Composite->pict matrix-module (Composite->place-spec matrix-module)))
