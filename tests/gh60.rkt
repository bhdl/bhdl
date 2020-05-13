#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         "../src/pict-utils.rkt"
         "../src/common.rkt"

         ;; DEBUG
         "../src/fp-kicad.rkt"
         "../src/library-io.rkt"
         pict
         json)

(define global
  (let ([res (Atom (make-hash '()))])
    (hash-set! (Atom-pinhash res) 'VCC (Pin res 1))
    (hash-set! (Atom-pinhash res) 'GND (Pin res 2))
    (hash-set! (Atom-pinhash res) '5V (Pin res 3))
    (hash-set! (Atom-pinhash res) '3V3 (Pin res 4))
    res))

(define power-module
  (let ([c1 (C '1u)]
        [c2 (C '100n)]
        [c3 (C '100n)]
        [c4 (C '100n)]
        [c5 (C '100n)])
    (hook #:pins ()
          (global.VCC c1.1 c2.1 c3.1 c4.1 c5.1)
          (global.GND c1.2 c2.2 c3.2 c4.2 c5.2))))

(module+ test
  (*- (led 'green) (R '10k))
  (*- global.VCC (*- (led) (R '10k))))

(define io-module
  ;; GPIO 0-3
  (let ([res (create-simple-Composite 0 1 2 3)])
    (let ([backlight (connector 6)])
      (hook! res
             (backlight.1 global.VCC)
             (backlight.2 res.0)
             (backlight.3 res.1)
             (backlight.4 res.2)
             (backlight.5 res.3)
             (backlight.6 global.GND)))
    (let ([universal (connector 6)])
      (hook! res
             (universal.1 res.0)
             (universal.2 res.1)
             (universal.3 res.2)
             (universal.4 res.3)
             (universal.5 global.VCC)
             (universal.6 global.GND)))
    ;; LEDs
    (combine-Composites-1
     res
     (*- global.VCC (*- (led) (R '10k)) res.1)
     (*- global.VCC (*< (*- (*< (led)
                                (led)
                                (led)) (R '10k))
                        (*- (*< (led)
                                (led)) (R '10k))
                        (*- (*< (led)
                                (led)) (R '10k))
                        (*- (*< (led)
                                (led)) (R '10k))) res.3)
     (*- global.VCC (*< (led)
                        (led)) (R '10k) res.0))))

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
     res
     ;; TODO not finished
     (*- res.COL1 (*<  (*- (led) res.ROW2)
                       (*- (led) res.ROW4)
                       (*- (led) res.ROW5))))))

(define matrix-module
  (let ([name-rows '((ESC 1 2 3 4 5 6 7 8 9 0 - = backspace)
                     (Tab Q W E R T y u i o p #\[ #\] #\\)
                     (caps a s d f g h j k l #\; #\' enter)
                     (lshift z x c v b n m #\, #\. #\/ rshift)
                     ;; FIXME some positions do not have keys
                     (mod1 mod2 mod3 space mod4 mod5 mod6 mod7))])
    ;; TODO compute the position by pict library
    (let* ([pict-rows (compose-pipe name-rows
                                    ;; FIXME use real footprint pict?
                                    ;; DESIGN or use larger separation
                                    #:..> (λ (x)
                                            ;; use real switch pict
                                            (ghost (footprint->pict
                                                    (fp-switch-keyboard 1 'pcb)))))]
           ;; combine the pict rows
           [one-pict (compose-pipe pict-rows
                                   #:.> (λ (x) (apply hc-append 30 x))
                                   #:> (λ (x) (apply vc-append 30 x)))]
           ;; find
           [locs (compose-pipe pict-rows
                               #:.*> (λ (x) (let-values ([(x y) (cc-find one-pict x)])
                                              (Point x y))))]
           ;; create atoms
           [atom-rows (compose-pipe name-rows
                                    locs
                                    ;; FIXME seems to be using different kinds
                                    ;; of switches
                                    #:.*> (λ (name loc)
                                            ;; assign locations
                                            (loced-atom! (cherry) loc)))]
           [res (create-simple-Composite
                 row1 row2 row3 row4 row5
                 col1 col2 col3 col4 col5 col6 col7
                 col8 col9 col10 col11 col12 col13 col14)])
      ;; connect the atoms
      ;;
      ;; FIXME performance
      (combine-Composites-1
       res
       (apply combine-Composites
              (for/list ([atoms atom-rows]
                         [i '(row1 row2 row3 row4 row5)])
                (apply combine-Composites
                       (for/list ([atom atoms]
                                  [j '(col1 col2 col3 col4 col5 col6 col7
                                            col8 col9 col10 col11 col12 col13 col14)])
                         (*-
                          ;; col
                          (pin-ref res j)
                          ;; TODO I actually want to assign larger weight to this link
                          (diode) #:weight 2 atom
                          ;; row
                          (pin-ref res i))))))))))

(define ic-module
  ;; ic
  (let ([ic (make-IC-atom ATmega32U4)]
        [res (create-simple-Composite)])
    (hook! res
           (ic.VCC global.VCC)
           (ic.GND global.GND))

    ;; connect IO module
    (hook! res
           (ic.PF7 io-module.0)
           (ic.PF6 io-module.1)
           (ic.PF5 io-module.2)
           (ic.PF4 io-module.3))
    ;; TODO connect led
    ;; TODO connect matrix
    (hook! res
           ;; FIXME syntax sugar for vectorized connections
           (matrix-module.row1 ic.PD0)
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
    (let ([usb (usb 'a-male)]
          [r1 (R 22)]
          [r2 (R 22)]
          [c (C '1u)])
      (hook! res
             (ic.UVCC ic.VBUS global.VCC)
             (ic.UGND global.GND)
             (ic.UCAP c.1) (c.2 global.GND)
             (usb.VBUS global.VCC)
             (usb.D- r1.1) (r1.2 ic.D-)
             (usb.D+ r2.1) (r2.2 ic.D+)
             (usb.GND global.GND)))
    ;; crystal module
    ;;
    ;; FIXME crystal type
    ;; FIXME general crystal constructor
    (let ([x (make-simple-atom Atom 4)]
          [c1 (C '22p)]
          [c2 (C '22p)])
      (hook! res
             (ic.XTAL1 c2.1) (c2.2 global.GND)
             (ic.XTAL2 c1.1) (c1.2 global.GND)
             (ic.XTAL1 x.1) (ic.XTAL2 x.2) (x.3 x.4 global.GND)))

    ;; ICSP module
    ;;
    ;; FIXME switch type
    (let ([sw (make-simple-atom Atom 4)]
          [icsp (connector 6)]
          [r (R '10k)])
      (hook! res
             (ic.RESET sw.3) (sw.1 global.GND)
             (global.VCC r.1) (r.2 ic.RESET)
             (icsp.1 ic.MISO)
             (icsp.2 global.VCC)
             ;; FIXME inconsistent naming: SCLK and SCK
             (icsp.3 ic.SCLK)
             (icsp.4 ic.MOSI)
             (icsp.5 ic.RESET)
             (icsp.6 global.GND)))
    res))

(define whole
  (combine-Composites ic-module
                      matrix-module
                      power-module
                      io-module))

(module+ test
  (collect-all-atoms whole)
  (Composite-nets whole)
  (pict-height (footprint->pict (fp-switch-keyboard 1)))
  (define init-place (Composite->place-spec whole '(2000 1000)))
  (Composite->pict whole
                   '(2000 1000)
                   (hash-ref init-place 'xs)
                   (hash-ref init-place 'ys))

  ;; there seems to be a little off, i.e. fixed xs and ys changed a little
  (make-directory* "/tmp/rackematic/out/")
  (save-for-placement (Composite->place-spec whole '(2000 1000))
                      "/tmp/rackematic/out/gh60.json")
  (define place-result
    (send-for-placement
     (Composite->place-spec whole '(2000 1000))))
  
  (define place-result-2
    (call-with-input-file "/tmp/rackematic/out/gh60-sol.json"
      (λ (in) (string->jsexpr (port->string in)))))

  (save-file (Composite->pict whole
                              '(2000 1000)
                              (hash-ref place-result 'xs)
                              (hash-ref place-result 'ys))
             "gh60.pdf")

  (collect-all-atoms ic-module)
  (collect-all-atoms matrix-module)
  (Composite-pinhash power-module)
  (collect-all-atoms power-module))

(module+ test-kicad
  (define init-place (Composite->place-spec whole '(2000 1000)))
  (call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole '(2000 1000)
                                          (hash-ref init-place 'xs)
                                          (hash-ref init-place 'ys))
                    out))))

