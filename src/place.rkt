#lang racket

(require "sch.rkt"
         "common.rkt"
         "library-io.rkt"
         "utils.rkt"
         "pict-utils.rkt"
         "library-io.rkt"
         ;; https://docs.racket-lang.org/json/index.html
         json
         graph
         ;; https://docs.racket-lang.org/net/url.html
         net/url
         pict)

(provide (contract-out
          [Composite->place-spec (any/c any/c . -> . any)]
          [Composite->pict       (any/c any/c any/c any/c . -> . any)])

         save-for-placement
         send-for-placement)

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

(module+ test
  (require "library-IC.rkt")
  (require "library.rkt")
  (atom->fp-pict+Hlocs (make-IC-atom ATmega8U2))
  (atom->macro (make-IC-atom ATmega8U2))
  (atom->macro (make-IC-atom ATtiny25)))

(define (annotate-atoms atoms)
  "Return hash table from (atom . 1-based-index)"
  ;; annotate cells and macros
  ;; I actually only need to annotate atoms, and I can create one macro for each atom
  (for/hash ([atom atoms]
             [i (in-naturals)])
    (values atom (add1 i))))

(define (Composite->place-spec comp diearea)
  "generate directly xs, ys, ws, hs, mask, Es, diearea"
  (let* ([netlist (Composite->netlist comp)]
         [atoms (collect-all-atoms comp)]
         [Hatom=>idx (annotate-atoms atoms)])
    (let ([xs (for/list ([atom atoms]) (if (Atom-loc atom)
                                           (Point-x (Atom-loc atom))
                                           0))]
          [ys (for/list ([atom atoms]) (if (Atom-loc atom)
                                           (Point-y (Atom-loc atom))
                                           0))]
          [mask (for/list ([atom atoms]) (if (Atom-loc atom) 0 1))]
          [ws (for/list ([atom atoms])
                (exact->inexact (Macro-w (atom->macro atom))))]
          [hs (for/list ([atom atoms])
                (exact->inexact (Macro-h (atom->macro atom))))]
          [Es
           ;; Edge is list of nets. Each net is a list of nodes, a node is
           ;; (index offx offy)
           (for/list ([net netlist])
             ;; TODO weight
             (for/list ([pin (Net-pins net)])
               (let* ([atom (Pin-parent pin)]
                      ;; FIXME pin index might be symbol
                      [pin-index (Pin-index pin)]
                      [macro (atom->macro atom)]
                      [offset (hash-ref (Macro-Hlocs macro) pin-index)])
                 (list (hash-ref Hatom=>idx atom)
                       (exact->inexact (Point-x offset))
                       (exact->inexact (Point-y offset))))))])
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
              ([(name offset) (Macro-Hlocs macro)])
      (pin-over res
                (Point-x offset)
                (Point-y offset)
                (text name)))))


(define (Composite->graph comp Hpin=>xy)
  ;; return a list of edges
  (define g (weighted-graph/undirected '()))
  ;; add vertex
  (for ([pin (collect-all-pins comp)])
    (add-vertex! g pin))
  ;; add pins
  (for ([net (Composite->netlist comp)])
    (for* ([pin1 (Net-pins net)]
           [pin2 (Net-pins net)])
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

(define (Composite->pict comp diearea xs ys)
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
                            [macro (atom->macro atom)]
                            [offset (hash-ref (Macro-Hlocs macro) index)])
                       (match (hash-ref Hatom=>xy atom)
                         [(list x y)
                          (values pin
                                  (list
                                   ;; CAUTION this macro pin offset is not
                                   ;; centered, to keep consistent with gerber
                                   ;; file convention.
                                   (+ (- x (/ (Macro-w macro) 2))
                                      (Point-x offset))
                                   (+ (- y (/ (Macro-h macro) 2))
                                      (Point-y offset))))])))])
    (let ([res (for/fold ([die die])
                         ([atom atoms]
                          [x xs]
                          [y ys])
                 (let* ([m (atom->macro atom)]
                        [w (Macro-w m)]
                        [h (Macro-h m)])
                   (pin-over-cc die x y
                                (atom->fp-pict atom))))])
      ;; Draw airwires.  Construct graph using racket's graph library, and find
      ;; MST with distance as weights
      (let* ([g (Composite->graph comp Hpin=>xy)]
             [edges (min-st-kruskal g)])
        (let ([final-res (for/fold ([res res])
                                   ([edge edges])
                           (let ([src (first edge)]
                                 [dst (second edge)])
                             (match-let ([(list x1 y1) (hash-ref Hpin=>xy src)]
                                         [(list x2 y2) (hash-ref Hpin=>xy dst)])
                               ;; however, pip-line does not support styling
                               ;; (pin-over res x1 y1 (pip-line (- x2 x1) (- y2 y1) 0))
                               (let ([src-p (circle 0)]
                                     [dst-p (circle 0)])
                                 (pin-line (pin-over-cc
                                            (pin-over-cc res x1 y1 src-p)
                                            x2 y2 dst-p)
                                           src-p cc-find
                                           dst-p cc-find
                                           #:style 'long-dash)))))])
          ;; scale it down
          (let* ([w (pict-width final-res)]
                 [h (pict-height final-res)]
                 [factor (min 1 (/ 640 w) (/ 480 h))])
            (scale final-res factor)))))))
