#lang racket

(require "../place.rkt"
         "../schematic.rkt"
         "../sch-lib.rkt"
         "../utils.rkt"
         json)

(module+ test
  (define mycomp
    (let ([r1 (R 11)]
          [r2 (R 22)]
          [c1 (C 1)])
      (hook #:pins (OUT1 OUT2)
            (self.OUT1 r1.1)
            (r1.2 r2.1)
            (r2.2 c1.1)
            (c1.2 self.OUT2)))))

(myvoid
 ;; will expand to the following code
 (define mycomp
   (let ([r1 (R 11)]
         [r2 (R 22)]
         [c1 (C 1)])
     (let ([comp (Composite (make-hash) '())])
       ;; create pins that refer to comp itself
       (hash-set! (Composite-pinhash comp) 'OUT1 (Pin comp 'OUT1))
       (hash-set! (Composite-pinhash comp) 'OUT2 (Pin comp 'OUT2))
       ;; create connections
       (set-Composite-connections!
        comp (list (list (pin-ref comp 'OUT1) (pin-ref r1 1))
                   (list (pin-ref r1 2) (pin-ref r2 1))
                   (list (pin-ref r2 2) (pin-ref c1 1))
                   (list (pin-ref c1 2) (pin-ref comp 'OUT2))))
       comp))))

(myvoid
 ;; see inside the composite
 (Composite-pinhash mycomp)
 (Composite-connections mycomp)
 ;; test netlist generation
 (collect-all-composites mycomp)
 (hash-ref (Composite-pinhash mycomp) 'OUT1)
 (Composite->netlist mycomp))

(module+ test
  (define-values (macros cells nets)
    (netlist->three (Composite->netlist mycomp))))

(myvoid
 ;; make sure they are jsexpr
 (jsexpr? (serialize-macros macros))
 (jsexpr? (serialize-cells cells))
 (jsexpr? (serialize-nets nets))
 (jsexpr? (serialize-all macros cells nets))
 ;; convert to string/bytes
 (jsexpr->string (serialize-macros macros))
 (jsexpr->bytes (serialize-all macros cells nets)))

(myvoid
 ;; TODO parse results
 (send-for-placement macros cells nets)
 (save-for-placement macros cells nets "out/a.json"))

(module+ test
  ;; this is hash table from "cell name" to '(x y)
  (define result (string->jsexpr
                  (port->string
                   (open-input-file "out/a-sol.json"))))
  (visualize-placement macros cells nets result))

