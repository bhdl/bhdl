#lang s-exp "../src/splicing.rkt"

(require "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         "../src/pict-utils.rkt"
         "../src/common.rkt"

         "../src/place.rkt"

         "../src/fp-kicad.rkt"
         "../src/library-io.rkt"

         "../src/atom-pict-wrapper.rkt"
         (only-in pict
                  rectangle
                  ghost)

         json
         (for-syntax syntax/parse
                     racket/string))

(define uno1 (picted-atom! (make-IC-atom Arduino-Uno)))
(define uno2 (picted-atom! (make-IC-atom Arduino-Uno)))
(define uno3 (picted-atom! (make-IC-atom Arduino-Uno-ICSP)))
(define uno4 (picted-atom! (make-IC-atom Arduino-Uno-ICSP)))

(define micro (picted-atom! (make-IC-atom Arduino-Micro)))
(define mini (picted-atom! (make-IC-atom Arduino-Mini)))
(define nano (picted-atom! (make-IC-atom Arduino-Nano)))
(define mkr (picted-atom! (make-IC-atom Arduino-MKR)))

(define (add-ghost-top p sep)
  "Add some space on top of picture"
  (vc-append (ghost (rectangle 1 sep)) p))

(define-Composite whole
  ;; FIXME inset a little bit
  #:layout (let ([g1 (ct-superimpose
                      uno1
                      (vc-append
                       10
                       (add-ghost-top mini 50)
                       (vc-append (rotate jumper-mini-nano-SCL (/ pi 2))
                                  (rotate jumper-mini-nano-SDA (/ pi 2)))))]
                 [g2 (ct-superimpose
                      uno2
                      (vc-append
                       10
                       (add-ghost-top micro 50)
                       (vc-append (rotate jumper-micro-SDA (/ pi 2))
                                  (rotate jumper-micro-SCL (/ pi 2))))
                      (rotate jumper-micro-mini-VCC (/ pi 2)))]
                 [g3 (ct-superimpose
                      uno3
                      (hc-append 10
                                 (vc-append (rotate jumper-mkr-MOSI (/ pi 2))
                                            (rotate jumper-mkr-SCK (/ pi 2))
                                            (rotate jumper-mkr-SDA (/ pi 2))
                                            (rotate jumper-mkr-SCL (/ pi 2)))
                                 (add-ghost-top (rotate mkr pi) 50)))]
                 [g4 (ct-superimpose uno4 (add-ghost-top nano 50))])
             (vc-append 
              (ht-append 20 g1 g2)
              (ht-append 20 g3 g4)))
  #:vars ([jumper-mini-nano-SCL (picted-atom! (connector 3))]  ;j3
          [jumper-mini-nano-SDA (picted-atom! (connector 3))]  ;j4
          [jumper-micro-SDA (picted-atom! (connector 3))]      ;j10
          [jumper-micro-SCL (picted-atom! (connector 3))]      ;j11
          [jumper-micro-mini-VCC (picted-atom! (connector 3))] ;J1
          [jumper-mkr-MOSI (picted-atom! (connector 3))]       ;J5
          [jumper-mkr-SCK (picted-atom! (connector 3))]        ;J6
          [jumper-mkr-SDA (picted-atom! (connector 3))]        ;J8
          [jumper-mkr-SCL (picted-atom! (connector 3))]        ;J9
          )
  ;; The bunch of connections across all Arduinos
  #:connect
  (*=
   ;; TODO TX RX RESET VIN AREF IOREF
   (uno1  [A0 A1 A2 A3 A4 A5 -  -  -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 -   GND 3V3 5V])
   (uno2  [A0 A1 A2 A3 A4 A5 -  -  -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 -   GND 3V3 5V])
   (uno3  [A0 A1 A2 A3 A4 A5 -  -  -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 VCC GND 3V3 5V])
   (uno4  [A0 A1 A2 A3 A4 A5 -  -  -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 VCC GND 3V3 5V])
   (micro [A0 A1 A2 A3 -  -  -  -  -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 VCC GND -   -])
   (mini  [A0 A1 A2 A3 A4 A5 A6 A7 -  -  D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 VCC GND -   -])
   (nano  [A0 A1 A2 A3 A4 A5 A6 A7 D0 D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 -   GND 3V3 5V])
   ;; FIXME mkr doesn't have A7, but has D14
   (mkr   [A0 A1 A2 A3 A4 A5 A6 -  D0 D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13 -   GND 3V3 5V]))
  ;; TODO define some global signal for e.g. uno1.SDA
  #:connect (list (*= (jumper-mkr-SDA [1 2 3])
                      ([uno1.D11 mkr.D11 uno1.SDA]))
                  (*= (jumper-mkr-SCL [1 2 3])
                      ([uno1.D12 mkr.D12 uno1.SCL]))
                  (*= (jumper-mkr-MOSI [1 2 3])
                      ;; CAUTION only Uno with ICSP headers has the MOSI pin
                      ([uno3.D8 mkr.D8 uno3.MOSI]))
                  (*= (jumper-mkr-SCK [1 2 3])
                      ([uno3.D9 mkr.D9 uno3.SCK]))
                  (*= (jumper-micro-SDA [1 2 3])
                      ([uno1.D2 micro.D2 uno1.SDA]))
                  (*= (jumper-micro-SCL [1 2 3])
                      ([uno1.D3 micro.D3 uno1.SCL])))
  #:hooks ([jumper-mini-nano-SCL.2 mini.A5 nano.A5]
           [jumper-mini-nano-SCL.1 uno1.A5]
           [jumper-mini-nano-SCL.3 uno1.SCL]

           [jumper-mini-nano-SDA.2 mini.A4 nano.A4]
           [jumper-mini-nano-SDA.1 uno1.A4]
           [jumper-mini-nano-SDA.3 uno1.SDA]

           [jumper-micro-mini-VCC.2 micro.VCC mini.VCC]
           [jumper-micro-mini-VCC.1 uno1.3V3]
           [jumper-micro-mini-VCC.3 uno1.5V]))


(module+ test
  (Composite-pict whole)

  (define init-place (Composite->place-spec whole))
  (Composite->pict whole init-place)

  (nplaced-atoms whole)
  (nfree-atoms whole)
  (hash-ref init-place 'xs)

  ;; generate
  (save-file (Composite->pict whole init-place) "../out/out.pdf")
  ;; well I can directly write KiCAD file
  (call-with-output-file "../out/out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole init-place)
                    out)))
  (call-with-output-file "../out/out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole init-place)
                    out)))
  ;; call command line tool to do routing
  (current-directory)
  (parameterize ([current-directory "../out/"])
    ;; spreadboard is taking too much time to route, consider reduce the number
    ;; of passes
    (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 10"))
  (system "ls"))
