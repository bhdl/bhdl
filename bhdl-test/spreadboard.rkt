#lang s-exp bhdl/splicing

(require bhdl
         (prefix-in pict: pict))

(define (add-ghost-top p sep)
  "Add some space on top of picture"
  (vc-append (pict:ghost (pict:rectangle 1 sep)) p))

(define whole
  (make-circuit
   ;; FIXME inset a little bit
   #:vars ([jumper-mini-nano-SCL (PinHeader 3)]  ;j3
           [jumper-mini-nano-SDA (PinHeader 3)]  ;j4
           [jumper-micro-SDA (PinHeader 3)]      ;j10
           [jumper-micro-SCL (PinHeader 3)]      ;j11
           [jumper-micro-mini-VCC (PinHeader 3)] ;J1
           [jumper-mkr-MOSI (PinHeader 3)]       ;J5
           [jumper-mkr-SCK (PinHeader 3)]        ;J6
           [jumper-mkr-SDA (PinHeader 3)]        ;J8
           [jumper-mkr-SCL (PinHeader 3)]        ;J9
           [uno1 (Arduino-Uno)]
           [uno2 (Arduino-Uno)]
           [uno3 (Arduino-Uno-ICSP)]
           [uno4 (Arduino-Uno-ICSP)]
           [micro (Arduino-Micro)]
           [mini (Arduino-Mini)]
           [nano (Arduino-Nano)]
           [mkr (Arduino-MKR)])
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
                       ([uno1.D3 micro.D3 uno1.SCL]))
                   (*+ ([jumper-mini-nano-SCL.2 mini.A5 nano.A5]
                        [jumper-mini-nano-SCL.1 uno1.A5]
                        [jumper-mini-nano-SCL.3 uno1.SCL]

                        [jumper-mini-nano-SDA.2 mini.A4 nano.A4]
                        [jumper-mini-nano-SDA.1 uno1.A4]
                        [jumper-mini-nano-SDA.3 uno1.SDA]

                        [jumper-micro-mini-VCC.2 micro.VCC mini.VCC]
                        [jumper-micro-mini-VCC.1 uno1.3V3]
                        [jumper-micro-mini-VCC.3 uno1.5V])))
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
               (ht-append 20 g3 g4)))))

(module+ test
  (make-directory* "/tmp/bhdl/")
  (parameterize ([current-directory "/tmp/bhdl/"])
    (circuit-export whole #:auto-place #f #:formats '(kicad pdf dsn))))
