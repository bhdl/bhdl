
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/CPWtgTBEDKJd racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/CPP3pktxNiVc/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide atom->macro annotate-atoms Composite->place-spec save-for-placement placer-url send-for-placement send-for-global-place send-for-detailed-place send-for-global-and-detailed-place
    (struct-out Macro)
    
    )

    (require json
         graph
         net/url
         pict)

(struct Macro
  (w h
     ;; this Hlocs is from pin name to offset Point
     Hlocs)
  #:prefab)

(define (atom->macro atom)
  (let-values ([(pict Hlocs) (atom->fp-pict+Hlocs atom)])
    ;; CAUTION use the pict-height/width as macro size
    ;; FIXME this is exact, e.g. 6/5
    (let ([h (pict-height pict)]
          [w (pict-width pict)])
      ;; get the location of pins
      (Macro w h Hlocs))))

(define (annotate-atoms atoms)
  "Return hash table from (atom . 1-based-index)"
  ;; annotate cells and macros
  ;; I actually only need to annotate atoms, and I can create one macro for each atom
  (for/hash ([atom atoms]
             [i (in-naturals)])
    (values atom (add1 i))))


(define (Composite->place-spec 
          comp
          #:place-nsteps [place-nsteps 50]
          #:place-nbins[place-nbins 300]
          #:sa-ncycles [sa-ncycles 10]
          #:sa-nsteps [sa-nsteps 2000]
          #:sa-stepsize [sa-stepsize 10]
          ;; theta=0 means disable rotation
          #:sa-theta-stepsize [sa-theta-stepsize 0])
  (let* ([netlist (Composite->netlist comp)]
         [atoms (collect-all-atoms comp)]
         [Hatom=>idx (annotate-atoms atoms)]
         [diepict (Composite-pict comp)]
         [locs (for/list ([atom atoms])
                 (match-let
                   ([(list x y) 
                     (or 
                       (maybe-find cc-find diepict
                                   (Atom-pict atom))
                       ;; initially place to middle, for better
                       ;; visualization
                       (list 
                         (/ (pict-width diepict) 2)
                         (/ (pict-height diepict) 2)))]
                    [(list a) 
                     (or (maybe-find 
                           angle-find diepict 
                           (Atom-pict atom))
                         (list 0))])
                   (Point x y a)))])
    (let*-values 
      ([(xs ys as) 
        (for/lists (l1 l2 l3)
          ([loc locs])
          (match-let ([(Point x y a) loc])
                     (values x y a)))]
       [(mask) (for/list ([atom atoms])
                 (if (maybe-find cc-find diepict
                                 (Atom-pict atom))
                     ;; 0 for fixed
                     0 1))]
       [(ws hs) 
        (for/lists (l1 l2)
          ([atom atoms])
          (values (exact->inexact
                    (Macro-w (atom->macro atom)))
                  (exact->inexact 
                    (Macro-h 
                      (atom->macro atom)))))]
       ;; DEBUG CAUTION add some margin for better placement result
       ;; FIXME how to decide the size? I'll have to use mm or mil as unit
       [(ws hs) (values (map (lambda (x) 
                               (+ x (* (padding-general)
                                       (fp-scale)))) ws)
                        (map (lambda (x) 
                               (+ x (* (padding-general)
                                       (fp-scale)))) hs))]
       [(Es)
        ;; Edge is list of nets. Each net is a list of nodes, a node is
        ;; (index offx offy)
        (for/list ([net netlist])
          ;; TODO weight
          (for/list ([pin (Net-pins net)])
            (let* ([atom (Pin-parent pin)]
                   ;; FIXME pin index might be symbol
                   [pin-name (Pin-name pin)]
                   [macro (atom->macro atom)]
                   [offset (hash-ref (Macro-Hlocs macro) pin-name)])
              (list (hash-ref Hatom=>idx atom)
                    (exact->inexact (Point-x offset))
                    (exact->inexact (Point-y offset))))))])
      (hash 'xs xs
            'ys ys
            'as as
            'ws ws
            'hs hs
            'Es Es
            'diearea 
            (list (pict-width diepict)
                  (pict-height diepict))
            'place-params 
            (hash 'place-nsteps place-nsteps
                  'place-nbins place-nbins
                  'sa-ncycles sa-ncycles
                  'sa-nsteps sa-nsteps
                  'sa-stepsize sa-stepsize
                  'sa-theta-stepsize sa-theta-stepsize)
            'mask mask
            ;; initial empty conflicts. This will only be fill by the placement
            ;; engine
            'conflicts '()))))

(define (save-for-placement specs fname)
  (let ([tmp (make-temporary-file)])
    (call-with-output-file tmp
      (λ (out)
        (write-bytes
         (jsexpr->bytes specs)
         out))
      ;; make-temporary-file creates the file
      #:exists 'replace)
    ;; pretty print by python -m json.tool
    (let ([formatted
           (with-output-to-string
             (λ ()
               (shell (~a "python -m json.tool " tmp))))])
      (call-with-output-file fname
        (λ (out)
          ;; FIXME text output port?
          (write-string formatted out))
        #:exists 'replace))))

(define placer-url (make-parameter "http://localhost:8082"))

(define (send-for-placement specs)
  "FIXME detect whether the server is alive, and report meaningful errors and
recover with appropriate default."
  (let ([in (post-pure-port
             (string->url (placer-url))
             (jsexpr->bytes specs))])
    (begin0
        ;; TODO parse the placement results
        ;;
        ;; well, this has header. I need to remote the header, so maybe just use
        ;; pure port
        (string->jsexpr (port->string in))
      (close-input-port in))))

(define (send-for-global-place circuit)
  "Return global placement result."
  (send-for-placement (Composite->place-spec
                      circuit
                      #:place-nsteps 50
                      #:place-nbins 300
                        ;; do not do detailed placement
                      #:sa-ncycles 0)))

(define (send-for-detailed-place circuit global-place-result)
  "Given global placement result, run detailed placement."
  ;; modify (by-copy) the place result with updated place params
  (let* ([spec (Composite->place-spec
                 circuit
                 #:place-nsteps 0
                 ;; When cycle increases, the temperature cools down,
                 ;; and the later cycles are not very useful to
                 ;; remove conflicts. Thus, for this application, I
                 ;; might consider using only the first few cycles,
                 ;; and use a large number of steps (per cycle)
                 #:sa-ncycles 10
                 #:sa-nsteps 3000
                 #:sa-stepsize 10
                 ;; to support rotation, use non-0 e.g. 0.3
                 #:sa-theta-stepsize 0)]
         [spec (hash-set* 
                 spec
                 'xs (hash-ref global-place-result 'xs)
                 'ys (hash-ref global-place-result 'ys)
                 'as (hash-ref global-place-result 'as))])
    (send-for-placement spec)))

(define (send-for-global-and-detailed-place circuit)
  "Return global placement result."
  (send-for-placement (Composite->place-spec
                      circuit
                      #:place-nsteps 50
                      #:place-nbins 300
                        ;; do not do detailed placement
                      #:sa-ncycles 10
                      #:sa-nsteps 3000
                      #:sa-stepsize 10
                      ;; to support rotation, use non-0 e.g. 0.3
                      #:sa-theta-stepsize 0)))
  )
    