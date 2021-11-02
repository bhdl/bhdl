
(module ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPgPrJLFpatC/CP6M6tV7VJkb racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/main.rkt" "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt")
  (provide set-default-pict! make-IC-atom
    
    
    )

    (require (for-syntax syntax/parse)
         racket/contract
         pict)

(define (set-default-pict! atom)
  ;; I need to launder the pict becasue the footprint pict is cached. Otherwise
  ;; all the atoms with the same footprint will have the same location when upon
  ;; *-find function call
  (set-Atom-pict! atom (launder (atom->fp-pict atom))))

(define (make-IC-atom ic which-fp attrs)
  (let* ([fpspec (ic-select-fpspec ic which-fp)]
         [pins (FpSpec-pins fpspec)]
         [alts (IC-alts ic)])
    ;; this is alts extended with all pins not recorded in original alts
    (let ([alts (append (map list (set-subtract pins (flatten alts)))
                        alts)])
      (let ([comp (ICAtom (make-hash) ic which-fp attrs)])
        ;; HACK is all the atoms are ICAtom, and all created here, I can just activate the hashcode here.
;;         (debug "make-ICAtom" (eq-hash-code comp))
        (eq-hash-code comp)
        ;; each alt group
        (for ([alt alts]
              [pin-index (in-naturals 1)])
          (let* ([pin-name (string->symbol (~a "index-" pin-index))]
                 ;; TODO I actually want to create pins with the order of FpSpec-pins
                 [p (Pin comp pin-name)])
            ;; each pin name in the alt group
            (hash-set! (Atom-pinhash comp) pin-name p)
            (for ([a alt])
              (hash-set! (Atom-pinhash comp) a p))))

        ;; get the footprint and assign the footprint pin name to hash as well
        (when (IC-left ic)
          (hash-set! (Atom-pinhash comp)
                     'left
                     (hash-ref (Atom-pinhash comp) (IC-left ic))))
        (when (IC-right ic)
          (hash-set! (Atom-pinhash comp)
                     'right
                     (hash-ref (Atom-pinhash comp) (IC-right ic))))

        ;; the fp names
        ;;
        ;; FIXME well, why do I need to fp names?
        ;;
        ;; Because when generating KiCAD file, I need to figure out which pad is
        ;; connected to which net.
        (let ([pad-names (filter-not string? (map pad-spec-name (footprint-pads (FpSpec-fp fpspec))))])
          
          (or (= (length pins) (length pad-names))
              (begin
               (debug "IC-name:" (IC-name ic)
                      "\npins:"
                     pins
                     "\npad names:"
                     pad-names)
               ;; pairing them up
               (pretty-print (for/list ([i (in-range (max (length pins) (length pad-names)))]
                                 [pin (append pins (make-list (length pad-names) ""))]
                                 [pad (append pad-names (make-list (length pins) ""))])
                                (cons pin pad)))
               (error "pins and pad-names do not match: "
                     (length pins) (length pad-names))))
          ;; printing pairs for debugging
;;           (debug "IC-name:" (IC-name ic)
;;                  "\npairs:"
;;                  (map cons pins pad-names))
          (for ([pin pins]
                [pad pad-names])
            (hash-set! (Atom-pinhash comp)
                       (string->symbol (~a "fp-" pad))
                       (hash-ref (Atom-pinhash comp) pin))))

        ;; return the created Atom instance
        (set-default-pict! comp)
        comp))))
  )
    