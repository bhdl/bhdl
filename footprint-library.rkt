#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         "footprint.rkt"
         "gerber.rkt"
         pict
         racket/trace
         racket/draw)

(provide (struct-out footprint)
         (struct-out line-spec)
         (struct-out pad-spec)
         read-kicad-mod

         kicad-mounting-hole)


(define kicad-footprint-path "/home/hebi/github/reading/kicad-footprints/")

(define-syntax (kicad-helper stx)
  (syntax-parse stx
    [(_ s ...)
     #'(read-kicad-mod
        (string-append kicad-footprint-path s ...))]))

(define kicad-resistor-0603
  (kicad-helper "Resistor_SMD.pretty/"
                "R_0603_1608Metric.kicad_mod"))
(define kicad-resistor-0805
  (kicad-helper "Resistor_SMD.pretty/"
                "R_0805_2012Metric.kicad_mod"))
;; this is one of vertical mounting. I'm only using SMD for now
(define kicad-resistor-tht
  (kicad-helper "Resistor_THT.pretty/"
                "R_Axial_DIN0204_L3.6mm_D1.6mm_P2.54mm_Vertical.kicad_mod"))

;; FIXME This seems to be the same as resistor's. Then I'll define
;; only one.
(define kicad-capacitor-0603
  (kicad-helper "Capacitor_SMD.pretty/"
                "C_0603_1608Metric.kicad_mod"))
(define kicad-capacitor-0805
  (kicad-helper "Capacitor_SMD.pretty/"
                "C_0805_2012Metric.kicad_mod"))

(module+ test
  (footprint->pict kicad-resistor-0603)
  (footprint->pict kicad-resistor-0805)
  (footprint->pict kicad-resistor-tht)
  (footprint->pict kicad-capacitor-0603)
  (footprint->pict kicad-capacitor-0805))

;; only THT, SPST, slide switches
(define (kicad-sw-spst ct)
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

(module+ test
  (for/list ([ct '(1 2 3 4 6 8)])
    (footprint->pict (kicad-sw-spst ct))))

(define kicad-sw-push (kicad-helper "Button_Switch_THT.pretty/"
                                    "SW_PUSH_6mm.kicad_mod"))

(define kicad-jack-audio (kicad-helper "Connector_Audio.pretty/"
                                       "Jack_3.5mm_PJ311_Horizontal.kicad_mod"))
(define kicad-jack-barrel (kicad-helper "Connector_BarrelJack.pretty/"
                                        "BarrelJack_Horizontal.kicad_mod"))

(module+ test
  (footprint->pict kicad-sw-push)
  (footprint->pict kicad-sw-push)
  (footprint->pict kicad-jack-audio)
  (footprint->pict kicad-jack-barrel))

(define (kicad-pin-header ct)
  ;; available ct: 1,2,3,4,5,6,7,8
  (kicad-helper "Connector_PinHeader_2.54mm.pretty/"
                (~a "PinHeader_1x"
                    (~r ct #:min-width 2 #:pad-string "0")
                    "_P2.54mm_Vertical.kicad_mod")))

(module+ test
  (for/list ([ct '(1 2 3 4 5 6 7 8)])
    (footprint->pict (kicad-pin-header ct))))


(define kicad-usb-c-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_C_Plug_Molex_105444.kicad_mod"))
(define kicad-usb-c-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_C_Receptacle_Palconn_UTC16-G.kicad_mod"))
;; FIXME usb3 different?
(define kicad-usb-a-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_A_CNCTech_1001-011-01101_Horizontal.kicad_mod"))
(define kicad-usb-a-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_A_Molex_105057_Vertical.kicad_mod"))
;; FIXME male or female, type and manufacture
(define kicad-usb-micro-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_Micro-B_Wuerth_629105150521.kicad_mod"))
(define kicad-usb-micro-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_Micro-B_Molex-105133-0001.kicad_mod"))
(define kicad-usb-mini-male
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Tensility_54-00023_Vertical.kicad_mod"))
(define kicad-usb-mini-female
  (kicad-helper "Connector_USB.pretty/"
                "USB_Mini-B_Lumberg_2486_01_Horizontal.kicad_mod"))

(module+ test
  (footprint->pict kicad-usb-c-male)
  (footprint->pict kicad-usb-c-female)
  (footprint->pict kicad-usb-a-male)
  (footprint->pict kicad-usb-a-female)
  (footprint->pict kicad-usb-micro-male)
  (footprint->pict kicad-usb-micro-female)
  (footprint->pict kicad-usb-mini-male)
  (footprint->pict kicad-usb-mini-female))

;; (footprint->pict (kicad-pin-header 4))
(define kicad-crystal
  (kicad-helper "Crystal.pretty/"
                "Resonator-2Pin_W10.0mm_H5.0mm.kicad_mod"))

(define kicad-diode
  (kicad-helper "Diode_THT.pretty/"
                "D_DO-35_SOD27_P7.62mm_Horizontal.kicad_mod"))

(define kicad-1602
  (kicad-helper "Display.pretty/"
                "LCD-016N002L.kicad_mod"))

(module+ test
  (footprint->pict kicad-crystal)
  (footprint->pict kicad-diode)
  ;; this is huge
  (scale (footprint->pict kicad-1602) 0.2))

(define (kicad-mounting-hole m)
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

(module+ test
  ;; (footprint->pict (kicad-mounting-hole 2))
  ;; (displayln (footprint->gerber (kicad-mounting-hole 2)))
  (for/list ([m '(2 2.5 3 4 5 6 8)])
    (footprint->pict (kicad-mounting-hole m))))

;; FIXME many variations
(define (kicad-QFN ct)
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
                [(72) '(10 6)])])
    (kicad-helper "Package_DFN_QFN.pretty/"
                  (~a "QFN-" ct "-1EP_"
                      (first data) "x" (first data)
                      "mm_P0.5mm_EP"
                      (second data) "x" (second data)
                      "mm.kicad_mod"))))

(module+ test
  (kicad-QFN 12)
  (for/list ([ct '(12 16 20 24 28 32 44 72)])
    (footprint->pict (kicad-QFN ct))))

(define (kicad-PQFP ct)
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

(module+ test
  (for/list ([ct '(44 80 100 112 144 256)])
    (footprint->pict (kicad-PQFP ct))))

(define (kicad-TQFP ct)
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

(module+ test
  (footprint->pict (kicad-TQFP 176))
  (for/list ([ct '(32 44 48 64 80 100 120 128 144 176)])
    (footprint->pict (kicad-TQFP ct))))

(define (kicad-DIP ct)
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

(module+ test
  (for/list ([ct '(4 6 8 10 12 14 16 18 20 22 24 28 32
                     40 42 48 64)])
    (footprint->pict (kicad-DIP ct))))


(define (kicad-SOIC ct)
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

(module+ test
  (for/list ([ct '(16 18 20 24 28)])
    (footprint->pict (kicad-SOIC ct))))

(define (kicad-TSSOP ct)
  

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

(module+ test
  (for/list ([ct '(8 14 16 20 24 28 30 32 38 48 56)])
    (footprint->pict (kicad-TSSOP ct))))

(define (kicad-switch-keyboard spacing pcb-or-plate)
  ;; SW_Cherry_MX_1.00u_PCB.kicad_mod
  ;; SW_Cherry_MX_1.00u_Plate.kicad_mod
  ;; SW_Cherry_MX_1.25u_PCB.kicad_mod
  ;; SW_Cherry_MX_1.25u_Plate.kicad_mod
  (unless (member spacing '(1 1.25 1.5 1.75 2 2.25 2.75 6.25))
    (error "Error: uknown spacing" spacing))
  (kicad-helper "Button_Switch_Keyboard.pretty/"
                (~a "SW_Cherry_MX_"
                    (~r spacing #:precision '(= 2))
                    "u_"
                    (case pcb-or-plate
                      [(pcb) "PCB"]
                      [(plate) "Plate"]
                      [else (error "Unknown pcb-or-place.")])
                    ".kicad_mod")))


(module+ test
  (for*/list ([spacing '(1 1.25 1.5 1.75 2 2.25 2.75 6.25)]
              [pcb-or-plate '(pcb plate)])
    (footprint->pict (kicad-switch-keyboard spacing pcb-or-plate))))
