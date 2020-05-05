#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/utils.rkt"
         (submod "../src/place.rkt" vis)
         json)

(module+ test
  (define comp
    (let ([r1 (R 11)]
          [r2 (R 22)]
          [c1 (C 1)])
      (hook #:pins (OUT1 OUT2)
            (self.OUT1 r1.1)
            (r1.2 r2.1)
            (r2.2 c1.1)
            (c1.2 self.OUT2))))
  (Composite-pinhash comp)
  (collect-all-atoms comp)
  (collect-all-pins comp))


(myvoid
 ;; will expand to the following code
 (define comp
   (let ([r1 (R 11)]
         [r2 (R 22)]
         [c1 (C 1)])
     (let ([res (Composite (make-hash) '())])
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
 (Composite-pinhash comp)
 (Composite-connections comp)
 ;; test netlist generation
 (collect-all-composites comp)
 (hash-ref (Composite-pinhash comp) 'OUT1)
 (Composite->netlist comp))

(myvoid
 ;; TODO parse results
 ;;
 ;; TODO I actually want to save this so that I can process the results without
 ;; another placement
 ;;
 ;; for local placement debug purpose
 (make-directory* "/tmp/rackematic/out/")
 (save-for-placement (Composite->place-spec comp) "/tmp/rackematic/out/a.json")
 ;; send for replacement
 (define place-result (send-for-placement (Composite->place-spec comp)))

 ;; DEPRECATED save locally
 (call-with-output-file "/tmp/rackematic/out/a-sol.json"
   (λ (out)
     (write-string (jsexpr->string place-result) out))
   #:exists 'replace)
 ;; read it back
 (define place-result (call-with-input-file "/tmp/rackematic/out/a-sol.json"
                        (λ (in)
                          (string->jsexpr (port->string in))))))



(myvoid
 (Composite->pict comp
                  '(1000 1000)
                  (hash-ref place-result 'xs)
                  (hash-ref place-result 'ys)))

