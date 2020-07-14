#lang racket

(require bhdl
         (prefix-in pict: pict)
         json)

(define global
  (let ([res (Atom (make-hash '()))])
    (hash-set! (Atom-pinhash res) 'VCC (Pin res 1))
    (hash-set! (Atom-pinhash res) 'GND (Pin res 2))
    (hash-set! (Atom-pinhash res) '5V (Pin res 3))
    (hash-set! (Atom-pinhash res) '3V3 (Pin res 4))
    res))

(define-Composite power-module
  #:vars ([c1 (C '1u)]
           [c2 (C '100n)]
           [c3 (C '100n)]
           [c4 (C '100n)]
           [c5 (C '100n)])
  #:hooks ((global.VCC c1.1 c2.1 c3.1 c4.1 c5.1)
           (global.GND c1.2 c2.2 c3.2 c4.2 c5.2)))

(define-Composite io-module
  ;; GPIO 0-3
  #:external-pins (0 1 2 3)
  #:vars ([backlight (connector 6)])
  #:hooks ((backlight.1 global.VCC)
           (backlight.2 self.0)
           (backlight.3 self.1)
           (backlight.4 self.2)
           (backlight.5 self.3)
           (backlight.6 global.GND))
  #:vars ([universal (connector 6)])
  #:hooks ((universal.1 self.0)
           (universal.2 self.1)
           (universal.3 self.2)
           (universal.4 self.3)
           (universal.5 global.VCC)
           (universal.6 global.GND))
  ;; LEDs
  #:connect (list
             (*- global.VCC (*- (led) (R '10k)) self.1)
             (*- global.VCC (*< (*- (*< (led)
                                        (led)
                                        (led)) (R '10k))
                                (*- (*< (led)
                                        (led)) (R '10k))
                                (*- (*< (led)
                                        (led)) (R '10k))
                                (*- (*< (led)
                                        (led)) (R '10k))) self.3)
             (*- global.VCC (*< (led)
                                (led)) (R '10k) self.0)))

;; UPDATE it does not seem clear how lcolX and lrowX are connected. Thus I'm
;; skipping the led module
(define led-module
  ;; TODO duplicate matrix-module
  ;; FIXME seems to be a little bit different from matrix-module
  ;; FIXME I actually needs to have fixed locations that are relative to the keys
  (let ([res (create-simple-Composite
              ROW1 ROW2 ROW3 ROW4 ROW5
              COL1 COL2 COL3 COL4 COL5 COL6 COL7
              COL8 COL9 COL10 COL11 COL12 COL13 COL14)])
    (combine-Composites
     (list res
           ;; TODO not finished
           (*- res.COL1 (*<  (*- (led) res.ROW2)
                             (*- (led) res.ROW4)
                             (*- (led) res.ROW5)))))))

(define-Composite matrix-module
  #:external-pins (row1 row2 row3 row4 row5
                        col1 col2 col3 col4 col5 col6 col7
                        col8 col9 col10 col11 col12 col13 col14)
  #:layout one-pict
  ;; FIXME these are nested lists of Composites
  #:connect (for/list ([atoms atom-rows]
                       [i '(row1 row2 row3 row4 row5)])
              (for/list ([atom atoms]
                         [j '(col1 col2 col3 col4 col5 col6 col7
                                   col8 col9 col10 col11 col12 col13 col14)])
                (*-
                 ;; col
                 (pin-ref self j)
                 ;; TODO I actually want to assign larger weight to this link
                 (diode) atom
                 ;; row
                 (pin-ref self i))))
  #:vars ([name-rows '((ESC 1 2 3 4 5 6 7 8 9 0 - = backspace)
                        (Tab Q W E R T y u i o p #\[ #\] #\\)
                        (caps a s d f g h j k l #\; #\' enter)
                        (lshift z x c v b n m #\, #\. #\/ rshift)
                        ;; FIXME some positions do not have keys
                        (mod1 mod2 mod3 space mod4 mod5 mod6 mod7))]
           [pict-rows
            (compose-pipe
             name-rows
             ;; FIXME use real footprint pict?
             ;; DESIGN or use larger separation
             #:..> (λ (x)
                     ;; use real switch pict
                     ;;
                     ;; using frame purely for
                     ;; visualization for debugging
                     (pict:frame
                      (pict:ghost (footprint->pict
                              (fp-switch-keyboard 1 'pcb))))))]
           [one-pict (compose-pipe pict-rows
                                   #:.> (λ (x) (apply hc-append 30 x))
                                   #:> (λ (x) (apply vc-append 30 x)))]
           [atom-rows (compose-pipe name-rows
                                    pict-rows
                                    ;; FIXME seems to be using different kinds
                                    ;; of switches
                                    #:.*> (λ (name p)
                                            ;; assign locations
                                            (picted-atom! (cherry) p)))]))

;; Declare here so that I can access the pict and assign locations for them
(define ic (picted-atom! (make-IC-atom ATmega32U4)))
(define usb1 (picted-atom! (usb 'a-male)))

(define-Composite ic-module
  #:external-pins ()
  #:hooks ((ic.VCC global.VCC)
           (ic.GND global.GND))
  ;; connect IO module
  #:hooks ((ic.PF7 io-module.0)
           (ic.PF6 io-module.1)
           (ic.PF5 io-module.2)
           (ic.PF4 io-module.3))
  ;; TODO connect led
  ;; TODO connect matrix
  ;; FIXME syntax sugar for vectorized connections
  #:hooks ((matrix-module.row1 ic.PD0)
           (matrix-module.row2 ic.PD1)
           (matrix-module.row3 ic.PD2)
           (matrix-module.row4 ic.PD3)
           (matrix-module.row5 ic.PD5)
           (matrix-module.col1 ic.PF0)
           (matrix-module.col2 ic.PF1)
           (matrix-module.col3 ic.PE6)
           (matrix-module.col4 ic.PC7)
           (matrix-module.col5 ic.PC6)
           (matrix-module.col6 ic.PB6)
           (matrix-module.col7 ic.PD4)
           (matrix-module.col8 ic.PB1)
           (matrix-module.col9 ic.PB7)
           (matrix-module.col10 ic.PB5)
           (matrix-module.col11 ic.PB4)
           (matrix-module.col12 ic.PD7)
           (matrix-module.col13 ic.PD6)
           (matrix-module.col14 ic.PB3))
  ;; USB module
  #:vars ([r1 (R 22)]
           [r2 (R 22)]
           [c (C '1u)])
  #:hooks ((ic.UVCC ic.VBUS global.VCC)
           (ic.UGND global.GND)
           (ic.UCAP c.1) (c.2 global.GND)
           (usb1.VBUS global.VCC)
           (usb1.D- r1.1) (r1.2 ic.D-)
           (usb1.D+ r2.1) (r2.2 ic.D+)
           (usb1.GND global.GND))
  ;; crystal module
  ;;
  ;; FIXME crystal type
  ;; FIXME general crystal constructor
  #:vars ([x (make-simple-atom Atom 4)]
           [c1 (C '22p)]
           [c2 (C '22p)])
  #:hooks ((ic.XTAL1 c2.1)
           (c2.2 global.GND)
           (ic.XTAL2 c1.1) (c1.2 global.GND)
           (ic.XTAL1 x.1) (ic.XTAL2 x.2) (x.3 x.4 global.GND))
  ;; ICSP module
  ;;
  ;; FIXME switch type
  #:vars ([sw (make-simple-atom Atom 4)]
           [icsp (connector 6)]
           [r (R '10k)])
  #:hooks ((ic.RESET sw.3)
           (sw.1 global.GND)
           (global.VCC r.1) (r.2 ic.RESET)
           (icsp.1 ic.MISO)
           (icsp.2 global.VCC)
           ;; FIXME inconsistent naming: SCLK and SCK
           (icsp.3 ic.SCLK)
           (icsp.4 ic.MOSI)
           (icsp.5 ic.RESET)
           (icsp.6 global.GND)))

(define-Composite whole
  ;; CAUTION just to declare the pict
  #:layout (pict:inset (hb-append -100
                           (Atom-pict ic)
                           (Composite-pict matrix-module)
                           (Atom-pict usb1)) 200)
  #:connect (list ic-module
                  matrix-module
                  power-module
                  io-module))

(module+ test
  (Composite-pict matrix-module)
  (Composite-pict ic-module)
  (Composite-pict whole)

  (pict:cc-find (pict:inset (Composite-pict whole) 200)
           (Composite-pict whole))

  (collect-all-atoms whole)
  (Composite-nets whole)
  (define init-place (Composite->place-spec whole))

  (nplaced-atoms whole)
  (nfree-atoms whole)

  ;; FIXME It is broken, the cc-find is failing.
  ;;
  ;; (Composite->pict whole init-place)

  ;; ;; there seems to be a little off, i.e. fixed xs and ys changed a little
  ;; (make-directory* "/tmp/rackematic/out/")
  ;; (current-directory "/tmp/rackematic/out/")
  ;; (save-for-placement (Composite->place-spec whole)
  ;;                     "/tmp/rackematic/out/gh60.json")

  ;; (save-file (Composite->pict whole init-place)
  ;;            "gh60-init.pdf")

  ;; (collect-all-atoms ic-module)
  ;; (collect-all-atoms matrix-module)
  ;; (Composite-pinhash power-module)
  ;; (collect-all-atoms power-module)
  )

(module+ test-place
  (define place-result
    (send-for-placement
     ;; TODO the diearea should be specified only once
     (Composite->place-spec whole)))
  
  ;; save place result
  (call-with-output-file "/tmp/rackematic/out/gh60-sol.json"
    (λ (out)
      (write-string (jsexpr->string place-result) out))
    #:exists 'replace)

  ;; FIXME the order of the hash tables seems to be changing, as the saved
  ;; results cannot be used directly
  (define place-result-loaded
    (call-with-input-file "/tmp/rackematic/out/gh60-sol.json"
      (λ (in)
        (string->jsexpr (port->string in)))))

  ;; FIXME the whole circuits should be centered on the diearea
  (save-file (Composite->pict whole place-result)
             "gh60.pdf"))

(module+ test-kicad
  (define place-result
    (send-for-placement
     ;; TODO the diearea should be specified only once
     (Composite->place-spec whole
                            #:place-nsteps 50
                            #:place-nbins 300
                            ;; When cycle increases, the temperature cools down,
                            ;; and the later cycles are not very useful to
                            ;; remove conflicts. Thus, for this application, I
                            ;; might consider using only the first few cycles,
                            ;; and use a large number of steps (per cycle)
                            #:sa-ncycles 30
                            #:sa-nsteps 2000
                            #:sa-stepsize 10)))
  (save-file (Composite->pict whole place-result)
             "gh60.pdf")
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