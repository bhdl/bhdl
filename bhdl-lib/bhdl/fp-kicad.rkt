#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         "fp.rkt"
         "gerber.rkt"
         pict
         racket/trace
         racket/contract
         racket/draw
         json)

(provide kicad-footprint-paths
         (contract-out
          ;; constructors
          [fp-resistor ((or/c "0603" "0805" "THT") . -> . footprint?)]
          [fp-capacitor ((or/c "0603" "0805") . -> . footprint?)]
          [fp-sw-spst (-> (or/c 1 2 3 4 6 8) footprint?)]
          [fp-pin-header (-> (or/c 1 2 3 4 5 6 7 8) footprint?)]
          [fp-usb (-> (or/c 'a-male 'a-female
                            'c-male 'c-female
                            'micro-male 'micro-female
                            'mini-male 'mini-female)
                      footprint?)]
          [fp-mounting-hole (-> (or/c 2 2.5 3 4 5 6 8) footprint?)]
          [fp-QFN (-> (or/c 12 16 20 24 28 32 44 64 72) footprint?)]
          [fp-PQFP (-> (or/c 44 80 100 112 144 256) footprint?)]
          [fp-TQFP (-> (or/c 32 44 48 64 80 100 120 128 144 176) footprint?)]
          [fp-DIP (-> (or/c 4 6 8 10 12 14 16 18 20 22 24 28 32
                            40 42 48 64) footprint?)]
          [fp-LQFP (-> (or/c 36 44 48) footprint?)]
          [fp-SOIC (-> (or/c 16 18 20 24 28) footprint?)]
          [fp-TSSOP (-> (or/c 8 14 16 20 24 28
                              30 32 38 48 56) footprint?)]
          [fp-switch-keyboard (-> (or/c 1 1.25 1.5 1.75 2 2.25 2.75 6.25)
                                  (or/c 'pcb 'plate)
                                  footprint?)]

          [fp-Arduino (-> (or/c 'Micro 'Mini 'Nano 'MKR 'Uno 'Uno-ICSP) footprint?)]

          ;; direct footprints
          [fp-crystal footprint?]
          [fp-diode footprint?]
          [fp-sw-push footprint?]
          [fp-jack-audio footprint?]
          [fp-jack-barrel footprint?]
          [fp-1602 footprint?]
          [fp-esp32-wrover-e footprint?]))

(define (hash-ref-ref hash . keys)
  (for/fold ([acc hash])
      ([key keys])
    (hash-ref acc key)))

(define (read-easycad fname)
  "Read EasyCAD json file."
  (let ([jobj (call-with-input-file fname
                (lambda (in) (read-json in)))])
    (match-let* ([origin-x (10mil->mm (hash-ref-ref jobj 'head 'x))]
                 [origin-y (10mil->mm (hash-ref-ref jobj 'head 'y))]
                 [pre (hash-ref-ref jobj 'head 'c_para 'pre)]
                 [canvas (hash-ref jobj 'canvas)]
                 [shapes (hash-ref jobj 'shape)]
                 [tracks (filter (lambda (x) (string-prefix? x "TRACK")) shapes)]
                 [pads (filter (lambda (x) (string-prefix? x "PAD")) shapes)]
                 [(hash-table ('x x) ('y y) ('width width) ('height height))
                  (hash-ref jobj 'BBox)])
      ;; FIXME unit
      (let* ([line-specs (flatten (for/list ([track tracks])
                                    (parse-track track)))]
             [pad-specs (for/list ([pad pads])
                          (parse-pad pad))]
             [fn (lambda (item) (spec-offset item origin-x origin-y))])
        (footprint (map fn line-specs)
                   (map fn pad-specs))))))

(define (spec-offset spec offx offy)
  (match spec
    [(line-spec x1 y1 x2 y2 width)
     (line-spec (- x1 offx) (- y1 offy) (- x2 offx) (- y2 offy) width)]
    [(pad-spec num x y mounting-type shape size dsize)
     (pad-spec num (- x offx) (- y offy) mounting-type shape size dsize)]
    [else (error "spec-offset")]))

(define (group-by-index key lst)
  "Group lst by key(index)."
  (map (lambda (x) (map car x))
       (group-by (lambda (p)
                   (key (cdr p)))
                 (for/list ([x lst]
                            [i (in-naturals)])
                   (cons x i)))))

(define (group-by-2 lst)
  "group every 2 items"
  (group-by-index (lambda (idx) (floor (/ idx 2))) lst))

(module+ test
  (group-by-2 '(1 2 3 4 5 6)))

(define (10mil->mm x)
  (* 25.4 (/ (* x 10) 1000)))

(define (parse-track str)
  (match-let ([(list _ stroke layer net points ID _)
               (string-split str "~")])
    (let ([points (group-by-2 (string-split points))])
      (for/list ([a points]
                 [b (rest points)])
        (line-spec (10mil->mm (string->number (first a)))
                   (10mil->mm (string->number (second a)))
                   (10mil->mm (string->number (first b)))
                   (10mil->mm (string->number (second b)))
                   (10mil->mm (string->number stroke)))))))

(define (parse-pad str)
  (match-let ([(list _ shape x y
                     ;; seems to be in reverse order???
                     height width
                     layer net number
                     hole-radius points rotation ID hole-length hole-points _ _ _ _ _)
               (string-split str "~")])
    (pad-spec number
              (10mil->mm (string->number x))
              (10mil->mm (string->number y))
              ;; FIXME fixed smd
              'smd
              ;; FIXME shape corresponding to kicad
              (string->symbol (string-downcase shape))
              (list (10mil->mm (string->number width))
                    (10mil->mm (string->number height)))
              ;; hole? shape?
              (10mil->mm (string->number hole-radius)))))

(module+ test
  (parse-track "TRACK~1~3~S$216~4035.4331 2925.5906 3964.5669 2925.5906~gge219~0")
  (parse-track "TRACK~1~3~S$222~3970 2950 4010 2950 4010 2970 4030 2970 4030 3045 3970 3045 3970 2950~gge221~0")
  (parse-pad "PAD~RECT~3964.567~2955~3.5433~7.874~1~~1~0~3960.6299 2956.7717 3960.6299 2953.2283 3968.5039 2953.2283 3968.5039 2956.7717~90~gge5~0~~Y~0~~~3964.567,2955"))


(define fp-esp32-wrover-e
  ;; FIXME this is S2
  ;; (kicad-helper "RF_Module.pretty/ESP32-S2-WROVER.kicad_mod")
  ;;
  ;; TODO remove hard-coded path
  (read-easycad (expand-user-path "~/git/bhdl/bhdl-lib/bhdl/easyeda/WIFIM-SMD_ESP32-WROVER_2020-07-23_13-18-22.json")))

(module+ test
  (define fname "easyeda/WIFIM-SMD_ESP32-WROVER_2020-07-23_13-18-22.json")
  (define jobj (call-with-input-file fname
                 (lambda (in) (read-json in))))
  (read-easycad fname))

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
                   [`(fp_text ,_ ,text (at ,x ,y ,z ...) (layer ,l) ,other ...)
                    #f]
                   [`(fp_arc (start ,sx ,sy) (end ,ex ,ey)
                             (angle ,ag) (layer ,l) (width ,w))
                    #f]
                   [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                    (line-spec sx sy ex ey w)]
                   ;; FIXME optional z
                   [`(pad ,num ,mounting-type ,shape (at ,x ,y ,z ...)
                          (size ,s1 ,s2)
                          ;; FIXME optional dsize
                          (drill ,dsize) ... ,other-attrs ...)
                    (pad-spec num x y mounting-type shape (list s1 s2) dsize)]
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
                        (λ (x) (string? (pad-spec-num x)))
                        (filter pad-spec? specs))])
        (footprint line-specs pad-specs)))))

;; FIXME actually use this
;;
;; (kicad-footprint-paths
;;  '("/path/to/kicad-footprints"
;;    "/path/to/arduino-kicad-library/"
;;    "/path/to/SparkFun-KiCad-Libraries/Footprints/"))
(define kicad-footprint-paths
  (make-parameter #f))

(define (load-kicad-footprint-paths)
  (when (not (kicad-footprint-paths))
    (kicad-footprint-paths
     (string-split
      (or (getenv "BHDL_KICAD_FOOTPRINT_PATH")
          ;; TODO well, I could probably just download for user
          (error "BHDL: env variable BHDL_KICAD_FOOTPRINT_PATH is not set"))
      ":")))
  (kicad-footprint-paths))

(define (kicad-helper . lst)
  ;; libpath is a list of path
  ;;
  ;; FIXME the first match will be returned. This is problematic when different
  ;; path contains same name
  (or (for/or ([d (load-kicad-footprint-paths)])
        (let ([p (expand-user-path (apply build-path d lst))])
          (if (file-exists? p)
              (read-kicad-mod p)
              #f)))
      (error "Cannot find the kicad file for: " lst)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KiCAD official lib
;; https://github.com/KiCad/kicad-footprints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; small items: resistors, capacitors, switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; FIXME This seems to be the same as resistor's. Then I'll define
;; only one.
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

;; (footprint->pict (kicad-pin-header 4))
(define fp-crystal
  (kicad-helper "Crystal.pretty/"
                "Resonator-2Pin_W10.0mm_H5.0mm.kicad_mod"))

(define fp-diode
  (kicad-helper "Diode_THT.pretty/"
                "D_DO-35_SOD27_P7.62mm_Horizontal.kicad_mod"))

;; only THT, SPST, slide switches
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
                "USB_Micro-B_Molex-105133-0001.kicad_mod"))
(define fp-usb-mini-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Tensility_54-00023_Vertical.kicad_mod"))
(define fp-usb-mini-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Lumberg_2486_01_Horizontal.kicad_mod"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; other single items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fp-1602
  (kicad-helper "Display.pretty/"
                "LCD-016N002L.kicad_mod"))


(define (fp-mounting-hole m)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC footprints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME many variations
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

(module+ test
  (footprint-pads (fp-QFN 16))
  (for/list ([ct '(12 16 20 24 28 32 44 64 72)])
    (length (footprint-pads (fp-QFN ct)))))

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
  ;; LQFP-32_5x5mm_P0.5mm.kicad_mod
  ;; LQFP-32_7x7mm_P0.8mm.kicad_mod
  ;; LQFP-36_7x7mm_P0.65mm.kicad_mod
  ;; LQFP-44_10x10mm_P0.8mm.kicad_mod
  ;; LQFP-48_7x7mm_P0.5mm.kicad_mod
  ;; LQFP-52_10x10mm_P0.65mm.kicad_mod
  ;; LQFP-52_14x14mm_P1mm.kicad_mod
  ;; LQFP-64_10x10mm_P0.5mm.kicad_mod
  ;; LQFP-64_14x14mm_P0.8mm.kicad_mod
  ;; LQFP-64_7x7mm_P0.4mm.kicad_mod
  ;; LQFP-80_10x10mm_P0.4mm.kicad_mod
  ;; LQFP-80_12x12mm_P0.5mm.kicad_mod
  ;; LQFP-80_14x14mm_P0.65mm.kicad_mod
  ;; LQFP-100_14x14mm_P0.5mm.kicad_mod
  ;; LQFP-128_14x14mm_P0.4mm.kicad_mod
  ;; LQFP-128_14x20mm_P0.5mm.kicad_mod
  ;; LQFP-144_20x20mm_P0.5mm.kicad_mod
  ;; LQFP-160_24x24mm_P0.5mm.kicad_mod
  ;; LQFP-176_20x20mm_P0.4mm.kicad_mod
  ;; LQFP-176_24x24mm_P0.5mm.kicad_mod
  ;; LQFP-208_28x28mm_P0.5mm.kicad_mod
  ;; LQFP-216_24x24mm_P0.4mm.kicad_mod
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arduino lib
;; https://github.com/forrestbao/arduino-kicad-library
;; https://github.com/sparkfun/SparkFun-KiCad-Libraries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

