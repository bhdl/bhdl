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
  (make-Composite
   #:external-pins (GND 3V3 5V USB5V)))

(define (sw spacing)
  (footprint->pict
   (fp-switch-keyboard spacing 'pcb)))

(module+ test
  (fp-switch-keyboard 1 'pcb)
  (sw 1)
  (sw 1.5))

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
               [key-with-diode (make-Composite #:external-pins (p1 p2)
                                               #:vars ([d (Diode)])
                                               #:connect (*- atom d)
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
      (parameterize ([default-append-spacing 20])
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
                (- (/ pi 10)))))))

;; TODO I need to support two kinds of location.
;; 1. functional pict
;; 2. specify absolute location directly
;; 3. possibly mixing the two?

(define matrix-module
  (make-Composite
   ;; FIXME these should be row[5] col[14], or get from matrix
   #:external-pins (row1 row2 row3 row4 row5
                         col1 col2 col3 col4 col5 col6 col7
                         col8 col9 col10 col11 col12 col13 col14)
   #:layout (hc-append -150
                       (make-half 'left
                                  (for/list ([i (range 7)])
                                    (keyboard-col matrix i))
                                  ..)
                       (make-half 'right
                                  (for/list ([i (reverse (range 7 14))])
                                    (keyboard-col matrix i))
                                  ..))
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
                       ;; (pin-ref self yname)
                       ;; (diode)
                       key
                       ;; (pin-ref self xname)
                       ))))))))

(define gd32 (GD32VF103CBT6))

(define gd32-module
  (make-Composite
   ;; TODO signals to use in this circuit
   ;; #:signal ([GND 3V3])
   #:vars ([x2 (Crystal-2)]
           [x4 (Crystal-4)]
           [reg (ME6211C)]
           [usb (USB-C-16)]
           [rgb (TJ-S1615CY)]

           [btn-boot (SKRPACE010)]
           [btn-reset (SKRPACE010)])
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
              (*- global.3V3 (R '2k) (LED 'red) global.GND)
              (*- global.3V3 rgb.VIN)
              ;; FIXME the GPIO to use
              (*- rgb.R (R '4k7) gd32.PC13)
              (*- rgb.G (R '4k7) gd32.PA1)
              (*- rgb.B (R '4k7) gd32.PA2)

              ;; buttons
              ;;
              ;; FIXME write in one *-, and use in-place component
              (*- global.3V3 btn-boot.1)
              (*- btn-boot.3 gd32.BOOT0 (R '10k) global.GND)
              (*- gd32.BOOT1 (R '10k) global.GND)

              (*- gd32.NRST btn-reset.1)
              (*- btn-reset.3 global.GND))))

(define esp32 (ESP32-WROVER-E))

(define esp32-module
  (make-Composite
   #:vars ([usb (USB-C-16)]
           [cp2102n (CP2102N)]
           ;; FIXME BJT and pin name and order
           [q1 (SS8050-G)]
           [q2 (SS8050-G)]
           [reg (AMS1117-3.3)])
   ;; FIXME I intend this as internal signal name
   #:external-pins (EXT5V)
   #:connect (list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
              ;; usb connector
              (*- usb.VBUS (R '22.1k) cp2102n.VBUS)
              (*- usb.VBUS
                  ;; FIXME polarized and /-\
                  (Diode) self.EXT5V)
              (*- usb.GND global.GND)
              ;; data bus
              ;; FIXME diode: LESD5D5
              (*- usb.D+1 cp2102n.D+ (Diode) global.GND)
              (*- usb.D-1 cp2102n.D- (Diode) global.GND)
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
              (*- self.EXT5V (R '2k) (LED 'red) global.GND)
              (*- self.EXT5V (C '22uf) global.GND)
              (*- self.EXT5V reg.VIN)
              (*- reg.GND global.GND)
              (*- reg.VOUT (C '22uf) global.GND)

              ;; button
              (*- esp32.IO0 (*< (Switch)
                                (C '0.1uf))
                  global.GND)
              (*- esp32.EN (*< (Switch)
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
(define-Composite whole
  ;; CAUTION just to declare the pict
  #:layout (pict:inset (Composite-pict matrix-module) 100)
  #:connect (list matrix-module esp32-module gd32-module
                  ;; connect matrix module with gd32 module
                  (*= (matrix-module [row1 row2 row3 row4 row5])
                      (gd32 [PA0 PA1 PA2 PA3 PA4]))
                  (*= (matrix-module [col1 col2 col3 col4 col5 col6 col7
                                           col8 col9 col10 col11 col12 col13 col14])
                      (gd32 [PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7 PB8 PB9 PB10 PB11
                                 PA5 PA6]))
                  ;; connect esp32 and gd32 via SPI
                  (*= (gd32 [SPI1_CS SPI1_SCLK SPI1_MISO SPI1_MOSI])
                      (esp32 [VSPICS0 VSPICLK VSPIMISO VSPIMOSI]))))


;; visualizing init placement
(module+ test
  (make-directory* "/tmp/bhdl/")
  (current-directory "/tmp/bhdl/")

  (Composite-pict matrix-module)
  (Composite-nets matrix-module)

  (nplaced-atoms whole)
  (nfree-atoms whole)

  (Composite-pict whole)
  ;; TODO NOW rotation of fixed-location components
  (define init-place (Composite->place-spec whole))
  (length (collect-all-atoms whole))

  (save-file (Composite->pict whole init-place) "out.pdf")
  ;; well I can directly write KiCAD file
  (call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole init-place)
                    out)))
  (call-with-output-file "out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole init-place)
                    out)))
  ;; call command line tool to do routing
  (current-directory)
  (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 10")
  (system "ls"))

;; placement
(module+ test-placement
  (define place-spec
    (Composite->place-spec whole
                           #:place-nsteps 50
                           #:place-nbins 300
                           ;; When cycle increases, the temperature cools down,
                           ;; and the later cycles are not very useful to
                           ;; remove conflicts. Thus, for this application, I
                           ;; might consider using only the first few cycles,
                           ;; and use a large number of steps (per cycle)
                           #:sa-ncycles 10
                           #:sa-nsteps 3000
                           #:sa-stepsize 10
                           #:sa-theta-stepsize 0.3))
  ;; (save-for-placement place-spec "fitboard.json")

  (define place-result (send-for-placement place-spec))

  (save-file (Composite->pict whole place-result)
             "out.pdf")

  (call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole place-result)
                    out)))
  (call-with-output-file "out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole place-result)
                    out)))
  ;; call command line tool to do routing
  (current-directory)
  (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 5")
  (system "ls"))
