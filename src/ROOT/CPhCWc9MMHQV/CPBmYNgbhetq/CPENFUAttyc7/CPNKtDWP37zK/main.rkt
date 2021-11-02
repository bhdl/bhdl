
(module ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPNKtDWP37zK racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt")
  (provide lcsc->uuid lcsc->fp uuid->fp fp-kailh-socket-easyeda
    
    
    )

    (require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/trace
         racket/contract
         racket/match
         racket/draw
         racket/format
         json
         net/url)


(define (10mil->mm x)
  (* 25.4 (/ (* x 10) 1000)))

(define (adapt-unit x)
  (cond 
   [(string? x) (10mil->mm (string->number x))]
   [else (10mil->mm x)]))

(define (read-easyeda fname)
  "Read EasyEDA json file."
  (let ([jobj (call-with-input-file fname
                (lambda (in) (read-json in)))])
    (match-let* ([origin-x (adapt-unit (hash-ref-ref jobj 'head 'x))]
                 [origin-y (adapt-unit (hash-ref-ref jobj 'head 'y))]
                 [pre (hash-ref-ref jobj 'head 'c_para 'pre)]
                 [canvas (hash-ref jobj 'canvas)]
                 [shapes (hash-ref jobj 'shape)]
                 [tracks (filter (lambda (x) (string-prefix? x "TRACK")) shapes)]
                 [rects (filter (lambda (x) (string-prefix? x "RECT")) shapes)]
                 [pads (filter (lambda (x) (string-prefix? x "PAD")) shapes)]
                 [holes (filter (lambda (x) (string-prefix? x "HOLE")) shapes)]
                 [_ (for ([shape (filter-not (lambda (x) (or (string-prefix? x "TRACK")
                                                             (string-prefix? x "RECT")
                                                             (string-prefix? x "PAD")
                                                             (string-prefix? x "HOLE"))) shapes)])
                         ;; FIXME HOLEs
                         (debug "unrecognized shape: " (first (string-split shape "~"))))]
                 ;; FIXME more shapes, like "PL ..."
                 ;; FIXME report warnings for unrecognized shapes
                 [(hash-table ('x x) ('y y) ('width width) ('height height))
                  (hash-ref jobj 'BBox)])
      ;; FIXME unit
      (match-let* ([line-specs (flatten (list (for/list ([track tracks])
                                          (parse-track track))
                                        (for/list ([rect rects])
                                          (parse-rect rect))))]
             [pad-specs (for/list ([pad pads])
                                  (parse-pad pad))]
             [hole-specs (for/list ([hole holes])
                                   (parse-hole hole))]
             [fn (lambda (item) (spec-offset item origin-x origin-y))]
             [(list x1 y1 x2 y2) (get-4-corners (map fn line-specs))])
        (footprint (map fn line-specs)
                   (map fn pad-specs)
                   ;; by default, place at left
                   ;; FIXME hard-coded "3" what's the unit? mm?
                   (list (text-spec (- x1 3) (* 0.5 (+ y1 y2)))
                         (text-spec 0 0))
                   (map fn hole-specs))))))

(define (spec-offset spec offx offy)
  (match spec
    [(line-spec x1 y1 x2 y2 width)
     (line-spec (- x1 offx) (- y1 offy) (- x2 offx) (- y2 offy) width)]
    [(pad-spec name x y 0 mounting-type shape size dsize layer)
     (pad-spec name (- x offx) (- y offy) 0 mounting-type shape size dsize layer)]
    [else (error "spec-offset")]))

(define (parse-track str)
  (match-let ([(list _ stroke layer net points ID
                     ;; the last field seems to be optional, HACK using ID
                     ;; ... to handle the optional value
                     ...)
               (string-split str "~")])
    (let ([points (group-by-2 (string-split points))])
      (for/list ([a points]
                 [b (rest points)])
        (line-spec (adapt-unit (first a))
                   (adapt-unit (second a))
                   (adapt-unit (first b))
                   (adapt-unit (second b))
                   (adapt-unit stroke))))))

(define (parse-rect str)
  (match-let* ([(list _ x y w h other ...) (string-split str "~")]
               [(list x y w h) (map (lambda (x)
                                      (adapt-unit x))
                                    (list x y w h))]
               [(list x1 y1 x2 y2) (list x y (+ x w) (+ y h))]
               ;; FIXME there's no stroke in this command
               [stroke (adapt-unit 1)])
    ;; 4 segments
    ;; FIXME better use fp_poly
    ;; FIXME read fp_poly and others for kicad
    (list (line-spec x1 y1 x2 y1 stroke)
          (line-spec x2 y1 x2 y2 stroke)
          (line-spec x2 y2 x1 y2 stroke)
          (line-spec x1 y2 x1 y1 stroke))))

(define (->padname x)
  (or (string->number x)
      (string->symbol x)))

(define (parse-pad str)
  (match-let ([(list _ shape x y
                     ;; seems to be in reverse order???
                     width height
                     layer net name
                     hole-radius points rotation ID
                     ;; HACK
                     ;; ...
                     hole-length hole-points
                     other ...)
               (string-split str "~")])
    (let* ([type (if (= (string->number hole-length) 0) 'smd 'thru_hole)]
           [dsize (case type
                    [(smd) (list 0)]
                    [(thru_hole) (match-let ([(list x1 y1 x2 y2)
                                              (map string->number
                                                   (string-split hole-points))])
                                   (let ([w (adapt-unit (- x2 x1))]
                                         [h (adapt-unit (abs (- y2 y1)))])
                                     ;; CAUTION x2 seems to be reasonable size ..
                                     (list 'oval (adapt-unit
                                                    (* 2 (adapt-unit hole-radius)))
                                           (adapt-unit hole-length))))])]
           [layer (case (string->number layer)
                        [(1) 'top]
                        [(2) 'bottom]
                        [(11) 'multi]
                        [else (error "layer not 1,2,11: " layer)])])
      (pad-spec (->padname name)
                (adapt-unit x)
                (adapt-unit y)
                0
                type
                ;; FIXME shape corresponding to kicad
                ;; KiCAD has no ellipse pads, using oval instead
                (if (string=? (string-downcase shape) "ellipse") 
                    'oval
                    (string->symbol (string-downcase shape)))
                (list (adapt-unit width)
                      (adapt-unit height))
                ;; hole? shape?
                ;; d size is also a 2 size, same when round, differ when oval
                ;; (10mil->mm (string->number hole-radius))
                ;; not the hole-radius, but hole-points
                ;;
                ;; FIXME rotation seems to follow the pad
                dsize
                layer))))

(define (parse-hole str)
  (match-let ([(list _ x y d other ...)
               (string-split str "~")])
             ;; HACK a pad with "" as name is a hole, doesn't have electrical pads,
             ;; but only footprint. On the footprint, it should not be red.
             (pad-spec "" (adapt-unit x)
                              (adapt-unit y)
                       0
                       'thru_hole
                       'circle
                       (list (* 2 (adapt-unit d)) (* 2 (adapt-unit d)))
                       (list (* 2 (adapt-unit d)))
                       'multi)))

(define lcsc2uuid #f)
(define lcsc2uuid-json (build-path
                        (bhdl-footprints-path)
                        "easyeda" "lcsc2uuid.json"))

(define (lcsc->uuid lcsc-id)
  "From lcsc component ID number to footprint UUID"
  ;; only symbol is allowed as keys for json package to recognize it as jsexpr?
  (when (string? lcsc-id)
    (set! lcsc-id (string->symbol lcsc-id)))
  (when (not lcsc2uuid)
    (if (file-exists? lcsc2uuid-json)
        (set! lcsc2uuid
              (call-with-input-file lcsc2uuid-json
                (lambda (in)
                  (read-json in))))
        (set! lcsc2uuid (make-hash))))
  (when (not (hash-has-key? lcsc2uuid lcsc-id))
    ;; read from network
    ;; https://easyeda.com/api/products/C440457/svgs
    ;;
    ;; or probably use the CN site: https://lceda.cn/api/products/C114587/svgs
    (let* ([url (~a "https://easyeda.com/api/products/" lcsc-id "/svgs")]
           [_ (displayln (~a "requesting " url " .."))]
           [jobj (call/input-url (string->url url)
                                 get-pure-port
                                 (lambda (in)
                                   (read-json in)))]
           ;; extract information
           [uuid (hash-ref (list-ref (hash-ref jobj 'result) 1) 'component_uuid)])
      ;; save uuid
      ;;
      ;; CAUTION hash-set! does not work because the hashtable from read-json is
      ;; immutable
      (set! lcsc2uuid (hash-set lcsc2uuid lcsc-id uuid))
      (call-with-output-file lcsc2uuid-json
        (lambda (out) (write-json lcsc2uuid out))
        #:exists 'replace)))
  ;; FIXME this will through error if still not found
  (hash-ref lcsc2uuid lcsc-id))

(define (lcsc->fp lcsc-id)
  (uuid->fp (lcsc->uuid lcsc-id)))

(define (uuid->fp uuid)
  "Cached version of getting easyeda file."
  ;; 1. check cache to see if uuid already built
  ;; 2. check if we have record of which file the uuid is from
  ;; 1. get fname from uuid
  ;; 2. if fname exist locally, directly
  ;;
  ;; https://easyeda.com/api/components/3e67c3e3b97b4ff38f0c80567ed48498
  (let ([fname (build-path (bhdl-footprints-path) "easyeda" (~a uuid ".json"))])
    (when 
      (not (file-exists? fname))
      (let* ([url (~a "https://easyeda.com/api/components/" uuid)]
             [_ (displayln (~a "requesting " url " .."))]
             [obj (call/input-url 
                    (string->url url)
                    get-pure-port
                    (lambda (in)
                      (let ([j (read-json in)])
                        (or (hash-ref-ref-noerr 
                              j
                              'result
                              ;; FIXME seems to be inconsistent, some json doesn't have this
                              'packageDetail
                              'dataStr)
                            (hash-ref-ref 
                              j 'result 
                              'dataStr)))))])
            (call-with-output-file
              fname
              (lambda (out) (write-json obj out)))
            (debug "Saved to" fname)
            ;; TODO pretty print using python
            ;; (system (~a "python -m json.tool " fname))
            ))
    ;; at this point, the file should appear locally
    (read-easyeda fname)))

(define fp-kailh-socket-easyeda
  (uuid->fp "bd8c6c64dc7b4d18806bb8859f9f2606"))
  )
    