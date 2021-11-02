
(module ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPHKPFTgKmQp racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt")
  (provide fp-resistor fp-fuse fp-capacitor fp-crystal fp-diode fp-sw-spst fp-sw-push fp-jack-audio fp-jack-barrel fp-pin-header fp-pin-header-2 fp-usb fp-1602 fp-dummy fp-mounting-hole fp-switch-keyboard fp-QFN fp-PQFP fp-LQFP fp-TQFP fp-DIP fp-SOIC fp-TSSOP fp-SOT-23 fp-SOT-223 fp-SOT-23-5 fp-Arduino fp-smd-2520 fp-smd-2012-2p fp-WS2812B fp-stabilizer-2u fp-kailh-socket-kicad
    
    
    )

    bhdl-footprints-path

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/trace
         racket/match
         racket/port
         racket/contract
         racket/draw
         racket/string
         racket/function
         json)

(define (read-kicad-mod fname)
  "Read a kicad mod file, parse it, and return a footprint object."
  (let ([kicad-mod (call-with-input-file fname
                     (λ (in)
                       (call-with-input-string
                           ;; replace dangling . as "." because (1 . 2 3) is
                           ;; illegal to read in racket
                           (string-replace (port->string in) " ." " \".\"")
                         (lambda (in) (read in)))))])
    (let ([specs
           (match kicad-mod
             [(list 'module name layer body ...)
              (filter
               identity
               (for/list [(e body)]
                 (match e
                   ;; TODO
                   ;; FIXME optional z
                    [`(fp_text reference ,text (at ,x ,y ,z ...) (layer ,l) ,other ...)
                     ;; TODO fp_text reference get the location
                    (text-spec x y)]
                   [`(fp_text ,_ ,text (at ,x ,y ,z ...) (layer ,l) ,other ...)
                     ;; TODO fp_text reference get the location
                    #f]
                   [`(fp_arc (start ,sx ,sy) (end ,ex ,ey)
                             (angle ,ag) (layer ,l) (width ,w))
                    #f]
                   [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                    (line-spec sx sy ex ey w)]
                   ;; FIXME optional z
                   [`(pad "" np_thru_hole circle (at ,x ,y) (size ,s1 ,s2) (drill ,dsize) ,layer)
                     (pad-spec "" x y 0 'thru_hole 'circle (list s1 s2) (list dsize) 'multi)]
                   [`(pad ,name ,mounting-type ,shape (at ,x ,y ,a ...)
                          (size ,s1 ,s2)
                          ;; FIXME optional dsize
                          ;; FIXME Oval drill has (oval 1 2)
                          (drill ,dsize ...) ...
                          ;; example: (layers B.Cu B.Paste B.Mask)
                          (layers ,layers ...)
                          ,other-attrs ...)
                     ;; FIXME z is used for TQFP
                    (pad-spec name x y
                              (if (empty? a) 0 (first a))
                              mounting-type shape (list s1 s2) (if (not (empty? dsize))
                                                                            (first dsize)
                                                                            '())
                              (cond
                               [(member '*.Cu layers) 'multi]
                               [(member 'F.Cu layers) 'top]
                               [(member 'B.Cu layers) 'bottom]
                               [(member 'F.Paste layers)
                                (when (not (string-contains? 
                                            (path->string fname)
                                            ;; FIXME this file is buggy
                                            "Package_DFN_QFN.pretty/QFN"))
                                      (warn "Unknown layer" layers "from" fname))
                                'top]
                               [else (error "Unknown layer:" layers)])
                              )]
                   ;; TODO
                   [`(fp_circle ,other ...)
                    #f]
                   [`(tedit ,other ...) #f]
                   [`(descr ,other ...) #f]
                   [`(tags ,other ...) #f]
                   [`(model ,other ...) #f]
                   [`(attr ,other ...) #f])))])])
      (let ([line-specs (filter line-spec? specs)]
            [pad-specs (filter-not
                        ;; FIXME actually I'm removing empty strings, as those
                        ;; seem to be not useful
                        ;;
                        ;; most of the pin names are numbers, but some like
                        ;; USB-C are symbols
                        (λ (x) (string? (pad-spec-name x)))
                        (filter pad-spec? specs))]
            [text-specs (append (filter text-spec? specs) (list (text-spec 0 0)))]
            [hole-specs (filter (λ (x) (string? (pad-spec-name x)))
                                (filter pad-spec? specs))])
        ;; FIXME no holes for now
        (footprint line-specs pad-specs text-specs hole-specs)))))

(define kicad-footprint-paths
  (make-parameter (map (lambda (x)
                         (build-path (bhdl-footprints-path)
                                     x))
                       '("kicad-footprints"
                         "arduino-kicad-library"
                         "SparkFun-KiCad-Libraries/Footprints"
                         "."))))

(define (kicad-helper . lst)
  ;; libpath is a list of path
  ;;
  ;; FIXME the first match will be returned. This is problematic when different
  ;; path contains same name
  (or (for/or ([d (kicad-footprint-paths)])
        (let ([p (expand-user-path (apply build-path d lst))])
          (if (file-exists? p)
              (read-kicad-mod p)
              #f)))
      (error "Cannot find the kicad file for: " lst)))

(define fp-resistor-0603
  (kicad-helper "Resistor_SMD.pretty/"
                "R_0603_1608Metric.kicad_mod"))
(define fp-resistor-0805
  (kicad-helper "Resistor_SMD.pretty/"
                "R_0805_2012Metric.kicad_mod"))
;; this is one of vertical mounting. I'm only using SMD for now
(define fp-resistor-THT
  (kicad-helper "Resistor_THT.pretty/"
                "R_Axial_DIN0204_L3.6mm_D1.6mm_P2.54mm_Vertical.kicad_mod"))

(define (fp-resistor type)
  (case type
    [("0603") fp-resistor-0603]
    [("0805") fp-resistor-0805]
    [("THT") fp-resistor-THT]))

(define fp-fuse-1206
  (kicad-helper "Fuse.pretty" "Fuse_1206_3216Metric.kicad_mod"))

(define (fp-fuse type)
  (case type
    [("1206") fp-fuse-1206]
    [else (error "fp-fuse: " type)]))

(define fp-capacitor-0603
  (kicad-helper "Capacitor_SMD.pretty/"
                "C_0603_1608Metric.kicad_mod"))
(define fp-capacitor-0805
  (kicad-helper "Capacitor_SMD.pretty/"
                "C_0805_2012Metric.kicad_mod"))

(define (fp-capacitor type)
  (case type
    [("0603") fp-capacitor-0603]
    [("0805") fp-capacitor-0805]))

(define fp-crystal
  (kicad-helper "Crystal.pretty/"
                "Resonator-2Pin_W10.0mm_H5.0mm.kicad_mod"))

(define fp-diode
  (kicad-helper "Diode_THT.pretty/"
                "D_DO-35_SOD27_P7.62mm_Horizontal.kicad_mod"))

(define (fp-sw-spst ct)
  (let ([height (case ct
                  [(1) 4.72]
                  [(2) 7.26]
                  [(3) 9.8]
                  [(4) 12.34]
                  [(6) 17.42]
                  [(8) 22.5])])
    (kicad-helper "Button_Switch_THT.pretty/"
                  (~a "SW_DIP_SPSTx"
                      (~r ct #:min-width 2 #:pad-string "0")
                      "_Slide_9.78x"
                      height
                      "mm_W7.62mm_P2.54mm.kicad_mod"))))


(define fp-sw-push (kicad-helper "Button_Switch_THT.pretty/"
                                 "SW_PUSH_6mm.kicad_mod"))

(define fp-jack-audio (kicad-helper "Connector_Audio.pretty/"
                                    "Jack_3.5mm_PJ311_Horizontal.kicad_mod"))


(define fp-jack-barrel (kicad-helper "Connector_BarrelJack.pretty/"
                                     "BarrelJack_Horizontal.kicad_mod"))

(define (fp-pin-header ct)
  ;; available ct: 1,2,3,4,5,6,7,8
  (kicad-helper "Connector_PinHeader_2.54mm.pretty/"
                (~a "PinHeader_1x"
                    (~r ct #:min-width 2 #:pad-string "0")
                    "_P2.54mm_Vertical.kicad_mod")))



(define (fp-pin-header-2 ct)
  (kicad-helper "Connector_PinHeader_2.54mm.pretty/"
                (~a "PinHeader_2x"
                    (~r ct #:min-width 2 #:pad-string "0")
                    "_P2.54mm_Vertical.kicad_mod")))

(define (fp-usb type)
  (case type
    [(c-male) fp-usb-c-male]
    [(c-female) fp-usb-c-female]
    [(a-male) fp-usb-a-male]
    [(a-female) fp-usb-a-female]
    [(micro-male) fp-usb-micro-male]
    [(micro-female) fp-usb-micro-female]
    [(mini-male) fp-usb-mini-male]
    [(mini-female) fp-usb-mini-female]
    [else (error (~a "Unsupported usb type: " type))]))

(define fp-usb-c-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_C_Plug_Molex_105444.kicad_mod"))
(define fp-usb-c-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_C_Receptacle_Palconn_UTC16-G.kicad_mod"))

;; FIXME usb3 different?
(define fp-usb-a-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_A_CNCTech_1001-011-01101_Horizontal.kicad_mod"))
(define fp-usb-a-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_A_Molex_105057_Vertical.kicad_mod"))
;; FIXME male or female, type and manufacture
(define fp-usb-micro-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_Micro-B_Wuerth_629105150521.kicad_mod"))
(define fp-usb-micro-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_Micro-B_Molex-105017-0001.kicad_mod"))
(define fp-usb-mini-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Tensility_54-00023_Vertical.kicad_mod"))
(define fp-usb-mini-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Lumberg_2486_01_Horizontal.kicad_mod"))

(define fp-1602
  (kicad-helper "Display.pretty/"
                "LCD-016N002L.kicad_mod"))



;; CAUTION FIXME this is completely a dummy fp to ease development placeholders
(define fp-dummy fp-1602)

(define (fp-mounting-hole m)
  "Augmented mounting hole with 2 dummy pads."
  (merge-fp (fp-mounting-hole-raw m)
            (footprint '() 
                       (list ;; FIXME these dummy pads should not be generated to KiCAD
                            (pad-spec 'dummy 0 0 0 'thru_hole 'circle (list 0 0) (list 0 0) 'multi)
                            (pad-spec 'dummy 0 0 0 'thru_hole 'circle (list 0 0) (list 0 0) 'multi))
                       '() '())))

(define (fp-mounting-hole-raw m)
  ;; MountingHole_2.2mm_M2.kicad_mod
  ;; MountingHole_2.7mm_M2.5.kicad_mod
  ;; MountingHole_3.2mm_M3.kicad_mod
  ;; MountingHole_4.3mm_M4.kicad_mod
  ;; MountingHole_5.3mm_M5.kicad_mod
  ;; MountingHole_6.4mm_M6.kicad_mod
  ;; MountingHole_8.4mm_M8.kicad_mod
  (kicad-helper "MountingHole.pretty/"
                (~a "MountingHole_"
                    (case m
                      [(2) 2.2]
                      [(2.5) 2.7]
                      [(3) 3.2]
                      [(4) 4.3]
                      [(5) 5.3]
                      [(6) 6.4]
                      [(8) 8.4])
                    "mm_M"
                    m
                    ".kicad_mod")))

(define (fp-switch-keyboard spacing pcb-or-plate)
  ;; SW_Cherry_MX_1.00u_PCB.kicad_mod
  ;; SW_Cherry_MX_1.00u_Plate.kicad_mod
  ;; SW_Cherry_MX_1.25u_PCB.kicad_mod
  ;; SW_Cherry_MX_1.25u_Plate.kicad_mod
  (kicad-helper "Button_Switch_Keyboard.pretty/"
                (~a "SW_Cherry_MX_"
                    (~r spacing #:precision '(= 2))
                    "u_"
                    (case pcb-or-plate
                      [(pcb) "PCB"]
                      [(plate) "Plate"]
                      [else (error "Unknown pcb-or-place.")])
                    ".kicad_mod")))


(define (fp-QFN ct)
  ;; QFN-12-1EP_3x3mm_P0.5mm_EP1.65x1.65mm.kicad_mod
  ;; QFN-16-1EP_3x3mm_P0.5mm_EP1.45x1.45mm.kicad_mod
  ;; QFN-20-1EP_3.5x3.5mm_P0.5mm_EP2x2mm.kicad_mod
  ;; QFN-24-1EP_4x4mm_P0.5mm_EP2.65x2.65mm.kicad_mod
  ;; QFN-28-1EP_5x5mm_P0.5mm_EP3.35x3.35mm.kicad_mod
  ;; QFN-32-1EP_5x5mm_P0.5mm_EP3.1x3.1mm.kicad_mod
  ;; QFN-44-1EP_7x7mm_P0.5mm_EP5.15x5.15mm.kicad_mod
  ;; QFN-72-1EP_10x10mm_P0.5mm_EP6x6mm.kicad_mod
  (let ([data (case ct
                [(12) '(3 1.65)]
                [(16) '(3 1.45)]
                [(20) '(3.5 2)]
                [(24) '(4 2.65)]
                [(28) '(5 3.35)]
                [(32) '(5 3.1)]
                [(44) '(7 5.15)]
                [(64) '(9 4.7)]
                [(72) '(10 6)])])
    (let ([fp (kicad-helper "Package_DFN_QFN.pretty/"
                            (~a "QFN-" ct "-1EP_"
                                (first data) "x" (first data)
                                "mm_P0.5mm_EP"
                                (second data) "x" (second data)
                                "mm.kicad_mod"))])
      ;; adjust fp to remove the last pads
      (or (= (length (footprint-pads fp)) (add1 ct))
          (error "The QFN's kicad footprint is not CT+1"))
      (struct-copy footprint fp
                   [pads (drop-right (footprint-pads fp) 1)]))))

(define (fp-PQFP ct)
  ;; PQFP-44_10x10mm_P0.8mm.kicad_mod
  ;; PQFP-80_14x20mm_P0.8mm.kicad_mod
  ;; PQFP-100_14x20mm_P0.65mm.kicad_mod
  ;; PQFP-112_20x20mm_P0.65mm.kicad_mod
  ;; PQFP-144_28x28mm_P0.65mm.kicad_mod
  ;; PQFP-256_28x28mm_P0.4mm.kicad_mod
  (let ([data (case ct
                [(44) '(10 10 0.8)]
                [(80) '(14 20 0.8)]
                [(100) '(14 20 0.65)]
                [(112) '(20 20 0.65)]
                [(144) '(28 28 0.65)]
                [(256) '(28 28 0.4)])])
    (kicad-helper "Package_QFP.pretty/"
                  (~a "PQFP-" ct "_"
                      (first data) "x" (second data)
                      "mm_P" (third data) "mm.kicad_mod"))))

(define (fp-LQFP ct)

  (match-let ([(list width pitch)
               (case ct
                 ;; FIXME not all of them
                 ;; FIXME there can be multiple footprints for a ct
                 [(36) '(7 0.65)]
                 [(44) '(10 0.8)]
                 [(48) '(7 0.5)])])
    (kicad-helper "Package_QFP.pretty/"
                  (~a "LQFP-" ct "_" width "x" width
                      "mm_P" pitch "mm.kicad_mod"))))

(define (fp-TQFP ct)
  ;;TQFP-32_7x7mm_P0.8mm.kicad_mod
  ;;TQFP-44_10x10mm_P0.8mm.kicad_mod
  ;;TQFP-48_7x7mm_P0.5mm.kicad_mod
  ;;TQFP-64_10x10mm_P0.5mm.kicad_mod
  ;;TQFP-80_12x12mm_P0.5mm.kicad_mod
  ;;TQFP-100_14x14mm_P0.5mm.kicad_mod
  ;;TQFP-120_14x14mm_P0.4mm.kicad_mod
  ;;TQFP-128_14x14mm_P0.4mm.kicad_mod
  ;;TQFP-144_20x20mm_P0.5mm.kicad_mod
  ;;TQFP-176_24x24mm_P0.5mm.kicad_mod
  (let ([data (case ct
                ;; FIXME kicad file wrong
                [(32) '(7 0.8)]
                ;; kicad file wrong
                [(44) '(10 0.8)]
                ;; kicad file wrong
                [(48) '(7 0.5)]
                [(64) '(10 0.5)]
                ;; kicad file wrong
                [(80) '(12 0.5)]
                [(100) '(14 0.5)]
                ;; kicad file wrong
                [(120) '(14 0.4)]
                ;; kicad file wrong
                [(128) '(14 0.4)]
                [(144) '(20 0.5)]
                [(176) '(24 0.5)])])
    (kicad-helper "Package_QFP.pretty/"
                  (~a "TQFP-" ct "_"
                      (first data) "x" (first data)
                      "mm_P" (second data) "mm.kicad_mod"))))

(define (fp-DIP ct)
  ;; DIP-4_W7.62mm.kicad_mod
  ;; DIP-6_W7.62mm.kicad_mod
  ;; DIP-8_W7.62mm.kicad_mod
  ;; DIP-40_W15.24mm.kicad_mod
  ;; DIP-42_W15.24mm.kicad_mod
  ;; DIP-48_W15.24mm.kicad_mod
  ;; DIP-64_W15.24mm.kicad_mod
  (let ([width (case ct
                 [(4 6 8 10 12 14 16 18 20 22 24 28 32)
                  7.62]
                 [(40 42 48 64)
                  15.24])])
    (kicad-helper "Package_DIP.pretty/"
                  (~a "DIP-" ct "_W" width "mm.kicad_mod"))))


(define (fp-SOIC ct)
  ;; SOIC-8_3.9x4.9mm_P1.27mm.kicad_mod
  ;; SOIC-14_3.9x8.7mm_P1.27mm.kicad_mod
  ;; SOIC-16W_7.5x10.3mm_P1.27mm.kicad_mod
  ;; SOIC-18W_7.5x11.6mm_P1.27mm.kicad_mod
  ;; SOIC-20W_7.5x12.8mm_P1.27mm.kicad_mod
  ;; SOIC-24W_7.5x15.4mm_P1.27mm.kicad_mod
  ;; SOIC-28W_7.5x17.9mm_P1.27mm.kicad_mod
  (kicad-helper "Package_SO.pretty/"
                (~a "SOIC-" ct "W_7.5x"
                    (case ct
                      [(16) 10.3]
                      [(18) 11.6]
                      [(20) 12.8]
                      [(24) 15.4]
                      [(28) 17.9])
                    "mm_P1.27mm.kicad_mod")))


(define (fp-TSSOP ct)
  ;; TSSOP-56_6.1x14mm_P0.5mm.kicad_mod
  (let ([data (case ct
                [(8) '(4.4 3 0.65)]
                [(14) '(4.4 5 0.65)]
                [(16) '(4.4 5 0.65)]
                [(20) '(4.4 6.5 0.65)]
                [(24) '(4.4 7.8 0.65)]
                [(28) '(4.4 9.7 0.65)]

                [(30) '(4.4 7.8 0.5)]
                [(32) '(6.1 11 0.65)]
                [(38) '(6.1 12.5 0.65)]
                [(48) '(6.1 12.5 0.5)]
                [(56) '(6.1 14 0.5)])])
    (kicad-helper "Package_SO.pretty/"
                  (~a "TSSOP-" ct "_"
                      (first data) "x" (second data)
                      "mm_P" (third data) "mm.kicad_mod"))))

(define fp-SOT-23
  (kicad-helper "Package_TO_SOT_SMD.pretty" "SOT-23.kicad_mod"))



(define fp-SOT-223
  (kicad-helper "Package_TO_SOT_SMD.pretty" "SOT-223.kicad_mod"))

(define fp-SOT-23-5
  (kicad-helper "Package_TO_SOT_SMD.pretty" "SOT-23-5.kicad_mod"))

(define (fp-Arduino type)
  (case type
    ;; sparkfun boards
    [(Uno)
     (kicad-helper "Boards.pretty/"
                   "UNO_R3_SHIELD.kicad_mod")]
    [(Uno-ICSP)
     (kicad-helper "Boards.pretty/"
                   "UNO_R3_SHIELD_ICSP.kicad_mod")]
    [(Micro)
     (kicad-helper "Boards.pretty/"
                   "SPARKFUN_PRO_MICRO.kicad_mod")]
    [(Mini)
     (kicad-helper "Boards.pretty/"
                   "ARDUINO_PRO_MINI.kicad_mod")]
    ;; Arduino boards
    [(MKR)
     (kicad-helper "Arduino.pretty/" "Arduino_MKR.kicad_mod")]
    [(Nano)
     (kicad-helper "Arduino.pretty/" "Arduino_Nano_Socket.kicad_mod")]
    [else (error "Unsupported Arduino form factor.")]))

(define fp-smd-2520
  (kicad-helper "Crystal.pretty"
                "Crystal_SMD_2520-4Pin_2.5x2.0mm.kicad_mod"))

(define fp-smd-2012-2p
  (kicad-helper "Crystal.pretty"
                "Crystal_SMD_2012-2Pin_2.0x1.2mm.kicad_mod"))

(define fp-WS2812B
  (kicad-helper "LED_SMD.pretty" "LED_WS2812B_PLCC4_5.0x5.0mm_P3.2mm.kicad_mod"))

(define fp-stabilizer-2u
  (kicad-helper "keyswitches.pretty" "Stabilizer_MX_2u.kicad_mod"))

(define fp-kailh-socket-kicad
  (kicad-helper "keyswitches.pretty" "Kailh_socket_MX.kicad_mod"))
  )
    