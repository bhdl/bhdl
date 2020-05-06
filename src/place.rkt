#lang racket

(require "sch.rkt"
         "common.rkt"
         "library-symbol.rkt"
         "library-io.rkt"
         "utils.rkt"
         "library-io.rkt"
         ;; https://docs.racket-lang.org/json/index.html
         json
         graph
         ;; https://docs.racket-lang.org/net/url.html
         net/url
         pict)

(provide (contract-out
          [Composite->place-spec (any/c (or/c 'symbol 'fp) . -> . any)]
          ;; (Composite->pict comp diearea xs ys symbol-or-fp)
          [Composite->pict       (any/c any/c any/c any/c
                                        (or/c 'symbol 'fp) . -> . any)])

         save-for-placement
         send-for-placement)

(struct Macro
  (w h pins)
  #:prefab)

(struct MacroPin
  (name offx offy)
  #:prefab)

(define (atom->macro atom symbol-or-fp)
  (let-values ([(pict locs) (case symbol-or-fp
                              [(symbol) (atom->symbol-pict+locs atom)]
                              [(fp)     (atom->fp-pict+locs atom)])])
    ;; CAUTION use the pict-height/width as macro size
    ;; FIXME this is exact, e.g. 6/5
    (let ([h (pict-height pict)]
          [w (pict-width pict)])
      ;; get the location of pins
      (Macro w h
             (for/list ([loc locs]
                        [index (in-naturals 1)])
               (match loc
                 [(Point offx offy)
                  (MacroPin (~a "P" index)
                            offx
                            offy)]))))))

(module+ test
  (require "library-IC.rkt")
  (require "library.rkt")
  (atom->symbol-pict+locs (make-IC-atom ATMEGA8U2))
  (atom->fp-pict+locs (make-IC-atom ATMEGA8U2))
  (atom->macro (make-IC-atom ATMEGA8U2) 'symbol)
  (atom->macro (make-IC-atom ATtiny25) 'symbol)
  (void))

(define (annotate-atoms atoms)
  "Return hash table from (atom . 1-based-index)"
  ;; annotate cells and macros
  ;; I actually only need to annotate atoms, and I can create one macro for each atom
  (for/hash ([atom atoms]
             [i (in-naturals)])
    (values atom (add1 i))))

(define (Composite->place-spec comp symbol-or-fp)
  "generate directly xs, ys, ws, hs, mask, Es, diearea"
  (let* ([netlist (Composite->netlist comp)]
         [atoms (collect-all-atoms comp)]
         [Hatom=>idx (annotate-atoms atoms)])
    ;; FIXME fixed diearea
    (let ([diearea '(1000 1000)]
          [xs (for/list ([atom atoms]) 0)]
          [ys (for/list ([atom atoms]) 0)]
          [mask (for/list ([atom atoms]) 1)]
          [ws (for/list ([atom atoms])
                (exact->inexact (Macro-w (atom->macro atom symbol-or-fp))))]
          [hs (for/list ([atom atoms])
                (exact->inexact (Macro-h (atom->macro atom symbol-or-fp))))]
          [Es (for/list ([net netlist])
                (for/list ([pin net])
                  (let* ([atom (Pin-parent pin)]
                         ;; FIXME pin index might be symbol
                         [pin-index (Pin-index pin)]
                         [macro (atom->macro atom symbol-or-fp)]
                         [pin (list-ref (Macro-pins macro) (sub1 pin-index))])
                    (list (hash-ref Hatom=>idx atom)
                          (exact->inexact (MacroPin-offx pin))
                          (exact->inexact (MacroPin-offy pin))))))])
      (hash 'xs xs
            'ys ys
            'ws ws
            'hs hs
            'Es Es
            'diearea diearea
            'mask mask))))

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
    (let ([formatted (with-output-to-string
                       (λ ()
                         (system (~a "python -m json.tool " tmp))))])
      (call-with-output-file fname
        (λ (out)
          ;; FIXME text output port?
          (write-string formatted out))
        #:exists 'replace))))

(define (send-for-placement specs)
  (let ([in (post-pure-port
             (string->url "http://localhost:8081")
             (jsexpr->bytes specs))])
    (begin0
        ;; TODO parse the placement results
        ;;
        ;; well, this has header. I need to remote the header, so maybe just use
        ;; pure port
        (string->jsexpr (port->string in))
      (close-input-port in))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite->pict with placement results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; submodule declared by module* is typically used to provide extra
;; functionalities. But here I just want to use it to better organize my code.

(define (draw-macro macro)
  "DEPRECATED"
  (let ([res (rectangle (Macro-w macro) (Macro-h macro))])
    (for/fold ([res res])
              ([pin (Macro-pins macro)])
      (pin-over res
                (MacroPin-offx pin)
                (MacroPin-offy pin)
                (text (MacroPin-name pin))))))

(define (Composite->graph comp Hpin=>xy)
  ;; return a list of edges
  (define g (weighted-graph/undirected '()))
  ;; add vertex
  (for ([pin (collect-all-pins comp)])
    (add-vertex! g pin))
  ;; add pins
  (for ([net (Composite->netlist comp)])
    (for* ([pin1 net]
           [pin2 net])
      (when (not (equal? pin1 pin2))
        (match-let ([(list x1 y1) (hash-ref Hpin=>xy pin1)]
                    [(list x2 y2) (hash-ref Hpin=>xy pin2)])
          (add-edge! g pin1 pin2 (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))))))
  (get-vertices g)
  (edge-weight g
               (first (first (get-edges g)))
               (second (first (get-edges g))))
  ;; to run MST outside
  ;; (min-st-kruskal g)
  g)

(define (Composite->pict comp diearea xs ys symbol-or-fp)
  ;; 1. draw the macro of each atoms on the right location
  (let* ([atoms (collect-all-atoms comp)]
         [die (match diearea
                [(list w h) (rectangle w h)])]
         ;; atom position
         [Hatom=>xy (for/hash ([atom atoms]
                               [x xs]
                               [y ys])
                      (values atom (list x y)))]
         ;; pin positions
         [Hpin=>xy (for/hash ([pin (collect-all-pins comp)])
                     (let* ([atom (Pin-parent pin)]
                            [index  (Pin-index pin)]
                            [macro (atom->macro atom symbol-or-fp)]
                            [macropin (list-ref (Macro-pins macro) (sub1 index  ))])
                       (match (hash-ref Hatom=>xy atom)
                         [(list x y)
                          (values pin
                                  (list
                                   ;; FIXME + offset or - offset
                                   (+ x (MacroPin-offx macropin))
                                   (+ y (MacroPin-offy macropin))))])))])
    (let ([res (for/fold ([die die])
                         ([atom atoms]
                          [x xs]
                          [y ys])
                 (let* ([m (atom->macro atom symbol-or-fp)]
                        [w (Macro-w m)]
                        [h (Macro-h m)])
                   (pin-over die x y
                             ;; (rectangle w h)
                             ;; I actually should draw the symbol for schematic
                             (case symbol-or-fp
                               [(symbol) (atom->symbol-pict atom)]
                               [(fp)     (atom->fp-pict atom)])
                             
                             ;; (draw-macro m)
                             )))])
      ;; Draw airwires.  Construct graph using racket's graph library, and find
      ;; MST with distance as weights
      (let* ([g (Composite->graph comp Hpin=>xy)]
             [edges (min-st-kruskal g)])
        (for/fold ([res res])
                  ([edge edges])
          (let ([src (first edge)]
                [dst (second edge)])
            (match-let ([(list x1 y1) (hash-ref Hpin=>xy src)]
                        [(list x2 y2) (hash-ref Hpin=>xy dst)])
              ;; however, pip-line does not support styling
              ;; (pin-over res x1 y1 (pip-line (- x2 x1) (- y2 y1) 0))
              (let ([src-p (circle 0)]
                    [dst-p (circle 0)])
                (pin-line (pin-over
                           (pin-over res x1 y1 src-p)
                           x2 y2 dst-p)
                          src-p cc-find
                          dst-p cc-find
                          #:style 'long-dash)))))))))
