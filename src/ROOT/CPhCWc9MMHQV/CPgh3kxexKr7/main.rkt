
(module ROOT/CPhCWc9MMHQV/CPgh3kxexKr7 racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt" "../../../ROOT/CPhCWc9MMHQV/main.rkt")
  (provide 
    
    
    )

    (module+ test

      (+ 1 2)

; (require bhdl)

; git clone https://github.com/bhdl/bhdl
; raco pkg install https://github.com/bhdl/bhdl

(bhdl-footprints-path)

; (download-footprints)

; triangular
atom->fp-pict+Hlocs

Resistor

(define global
  (make-circuit
   #:external-pins (GND 3V3 5V VCC USB5V)))

; Atom
CODEPOD-link

(define (key-with-diode)
  (make-circuit 
   #:external-pins (left right)
   #:vars ([d (1N4148W)]
           [key (kailh-socket 1)])
   #:connect (*- self.left key d self.right)
   #:layout (vc-append key d)))

(show-layout (key-with-diode))

(define matrix
  (for/list ([row (in-range 4)])
            (for/list ([col (in-range 5)])
                      (key-with-diode))))

;; connect the matrix
(define matrix-module
  (make-circuit 
    #:external-pins (row[4] col[5])
    ;; col in, row out
    #:connect 
    (for*/list ([i (in-range 4)]
                [j (in-range 5)])
               (debug "connecting" i j)
               (*- self.col[j]
                   (list-ref (list-ref matrix i) j)
                   self.row[i]))
    #:layout (inset (apply vc-append
                      (for/list ([row matrix])
                        (apply hc-append row)
                        ))
                    20)))

(show-layout matrix-module)

; circuit-export
Composite->place-spec

(parameterize ([current-directory "./out/demo-board/matrix"]
               [padding-general 2])
    (circuit-export matrix-module
                    #:auto-place #f
                    #:formats '(kicad pdf png svg)))

(define (icsp-header)
   (make-circuit #:vars ([h (PinHeader2 3)])
                   #:external-pins (MOSI SCK MISO RESET VCC GND)
                   #:connect (*= (self (MISO VCC SCK MOSI RESET GND))
                                 (h [1 2 3 4 5 6]))
                   #:layout h))

(Atom-pinhash (PinHeader2 3))
; (pin-ref (PinHeader2 3) 4)

(icsp-header)

;; Now using the Atmega32u4 from the arduino board
(define mcu-module 
  (make-circuit 
 #:vars ([mcu (ATmega32U4 #:FP "TQFP-44")]
         [usb (USB-Micro)]
         [icsp (icsp-header)]
         [xtal (Crystal-4 '16MHz)])
 #:external-pins (row[4] col[5])
 #:layout (vc-append 50 (rotate usb pi) mcu icsp)
 ;; define what are the row and col scan pins
 #:connect (*= (self row [0 3])
               (self row [0 3])
               (mcu [PB0 PB4 PB5 PB6]))
 #:connect (*= (self col [0 4])
                 (mcu [PD0 PD1 PD2 PD3 PD4]))
   ;; use a Pin for blinking LED
   #:connect (*- mcu.PC6 (R '1k) (LED0603 'red) global.GND)
                    
 ;; connect the ICSP header
 #:connect (*= (mcu (MOSI SCLK MISO RESET VCC GND))
               (icsp (MOSI SCK MISO RESET VCC GND)))
 #:connect
 (list (*- mcu.VCC mcu.UVCC mcu.AVCC global.VCC)
         (*- mcu.GND mcu.UGND global.GND)
         ;; reset circuit and switch
         (*- mcu.RESET (*< (LL4148)
                           (R '10k)) global.5V)
       ;; FIXME [-1] because (Switch) is parsed as the index otherwise
         (*- mcu.RESET[-1] (Switch) global.GND)
       ;; capacitors
       (*- mcu.UCAP (C '1uf) global.GND)
       (*- global.VCC (*< (C '1uf)
                          (C '100nf)
                          (C '100nf)
                          (C '100nf)
                          (C '100nf))
           global.GND)
         ;; crystal
       (*- mcu.XTAL1 xtal.XIN)
       (*- xtal.XOUT mcu.XTAL2)
       (*- xtal.GND global.GND)
         (*- mcu.XTAL1 (C '22pf) global.GND)
         (*- mcu.XTAL2 (C '22pf) global.GND)
       ;; USB
       (*- mcu.D+ (R 22) usb.D+)
       (*- mcu.D- (R 22) usb.D-)
       ;; just use USB for supplying power
       (*- mcu.VBUS usb.VBUS global.VCC)
       (*- mcu.UGND usb.GND global.GND)
       )))

;; connect MCU
(define mcu-board
  (make-circuit 
   #:layout (inset (cb-superimpose mcu-module) 50 0)
   #:connect mcu-module))

;; "http://localhost:8082"
(placer-url)

(placer-url "http://bhdl-place:8082")

(show-layout mcu-board)

(parameterize ([current-directory "./out/demo-board/mcu-board"]
               [padding-general 2])
    (circuit-export mcu-board
                    #:auto-place #t
                    #:formats '(kicad pdf bom png)))

(CODEPOD-link "./out/demo-board/mcu-board/out.kicad_pcb")

(CODEPOD-link "./out/demo-board/mcu-board/out.pdf")

      )
  )
    