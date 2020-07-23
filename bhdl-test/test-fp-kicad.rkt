#lang racket

(require bhdl
         (prefix-in pict: pict))

(module+ test
  (for/list ([ct '(1 2 3 4 6 8)])
    (footprint->pict (fp-sw-spst ct))))


(module+ test
  (footprint->pict (fp-resistor "0603"))
  (footprint->pict (fp-resistor "0805"))
  (footprint->pict (fp-resistor "THT"))
  (footprint->pict (fp-capacitor "0603"))
  (footprint->pict (fp-capacitor "0805")))

(module+ test
  (footprint->pict fp-sw-push)
  (footprint->pict fp-sw-push)
  (footprint->pict fp-jack-audio)
  (footprint->pict fp-jack-barrel))

(module+ test
  (for/list ([ct '(1 2 3 4 5 6 7 8)])
    (footprint->pict (fp-pin-header ct))))

(module+ test
  (footprint->pict (fp-usb 'c-male))
  (footprint->pict (fp-usb 'c-female))
  (footprint->pict (fp-usb 'a-male))
  (footprint->pict (fp-usb 'a-female))
  (footprint->pict (fp-usb 'micro-male))
  (footprint->pict (fp-usb 'micro-female))
  (footprint->pict (fp-usb 'mini-male))
  (footprint->pict (fp-usb 'mini-female)))

(module+ test
  (footprint->pict fp-crystal)
  (footprint->pict fp-diode)
  ;; this is huge
  (pict:scale (footprint->pict fp-1602) 0.2))

(module+ test
  ;; (footprint->pict (fp-mounting-hole 2))
  ;; (displayln (footprint->gerber (fp-mounting-hole 2)))
  (for/list ([m '(2 2.5 3 4 5 6 8)])
    (footprint->pict (fp-mounting-hole m))))

(module+ test
  (footprint->pict (fp-QFN 12))
  (for/list ([ct '(12 16 20 24 28 32 44 72)])
    (footprint->pict (fp-QFN ct))))

(module+ test
  (for/list ([ct '(44 80 100 112 144 256)])
    (footprint->pict (fp-PQFP ct))))

(module+ test
  (footprint->pict (fp-TQFP 176))
  (for/list ([ct '(32 44 48 64 80 100 120 128 144 176)])
    (footprint->pict (fp-TQFP ct))))

(module+ test
  (for/list ([ct '(4 6 8 10 12 14 16 18 20 22 24 28 32
                     40 42 48 64)])
    (footprint->pict (fp-DIP ct))))

(module+ test
  (for/list ([ct '(16 18 20 24 28)])
    (footprint->pict (fp-SOIC ct))))

(module+ test
  (for/list ([ct '(8 14 16 20 24 28 30 32 38 48 56)])
    (footprint->pict (fp-TSSOP ct))))


(module+ test
  (for*/list ([spacing '(1 1.25 1.5 1.75 2 2.25 2.75 6.25)]
              [pcb-or-plate '(pcb plate)])
    (footprint->pict (fp-switch-keyboard spacing pcb-or-plate))))


(module+ test
  (footprint->pict (fp-Arduino 'Uno))
  (footprint->pict (fp-Arduino 'Uno-ICSP))
  (footprint->pict (fp-Arduino 'Micro))
  (footprint->pict (fp-Arduino 'Nano))
  (footprint->pict (fp-Arduino 'MKR))

  (for/list ([type '(Micro Mini Nano MKR Uno Uno-ICSP)])
    (footprint->pict (fp-Arduino type))))

(module+ test
  (pict:frame (footprint->pict fp-esp32-wrover-e)))

(module+ test
  (footprint->pict (fp-LQFP 36))
  (footprint->pict (fp-LQFP 44))
  (pict:scale (footprint->pict (fp-LQFP 48)) 10))
