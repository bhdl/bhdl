#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         "fp-base.rkt"
         "gerber.rkt"
         "utils.rkt"
         pict
         racket/trace
         racket/contract
         racket/draw
         json
         net/url)

(provide uuid->fp
         lcsc->fp
         lcsc->uuid
         
         fp-kailh-socket
         
         10mil->mm)

(define (10mil->mm x)
  (* 25.4 (/ (* x 10) 1000)))

(define (adapt-unit x)
  (cond 
   [(string? x) (10mil->mm (string->number x))]
   [else (10mil->mm x)]))

(define (get-corner lines x-or-y min-or-max)
  (apply min-or-max (for/list ([line lines])
                              (match-let ([(line-spec x1 y1 x2 y2 stroke) line])
                                         (case x-or-y
                                               [(x) (min-or-max x1 x2)]
                                               [(y) (min-or-max y1 y2)])))))

(define (get-4-corners lines)
  (list (get-corner lines 'x min)
        (get-corner lines 'y min)
        (get-corner lines 'x max)
        (get-corner lines 'y max)))

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
    [(pad-spec name x y mounting-type shape size dsize layer)
     (pad-spec name (- x offx) (- y offy) mounting-type shape size dsize layer)]
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
                type
                ;; FIXME shape corresponding to kicad
                (string->symbol (string-downcase shape))
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

;; "HOLE~284~255~4~gge5"
;; "HOLE~410~280~5.905~"
;; center x: 284
;; center y: 255
;; diameter: 4
;; id: gge5
(define (parse-hole str)
  (match-let ([(list _ x y d other ...)
               (string-split str "~")])
             ;; HACK a pad with "" as name is a hole, doesn't have electrical pads,
             ;; but only footprint. On the footprint, it should not be red.
             (pad-spec "" (adapt-unit x)
                              (adapt-unit y)
                       'thru_hole
                       'circle
                       (list (* 2 (adapt-unit d)) (* 2 (adapt-unit d)))
                       (list (* 2 (adapt-unit d)))
                       'multi)))

(module+ test
  (parse-track "TRACK~1~3~S$216~4035.4331 2925.5906 3964.5669 2925.5906~gge219~0")
  (parse-track "TRACK~1~3~S$222~3970 2950 4010 2950 4010 2970 4030 2970 4030 3045 3970 3045 3970 2950~gge221~0")
  (parse-track "TRACK~1~3~S$39~3998 2996 4002 2996~gge38~")
  ;; (parse-rect "RECT~406~220~105~52~1~gge32")
  (parse-rect "RECT~3964.5669~2970.7874~70.8661~24.8031~3~gge204~0~1~none~~~")
  (parse-pad "PAD~RECT~3964.567~2955~3.5433~7.874~1~~1~0~3960.6299 2956.7717 3960.6299 2953.2283 3968.5039 2953.2283 3968.5039 2956.7717~90~gge5~0~~Y~0~~~3964.567,2955")
  (parse-pad "PAD~OVAL~4019~3004~3.937~7.0866~11~~4~1.1811~4019 3002.4252 4019 3005.5748~0~gge88~5.5118~4018.9996 3005.5745 4018.9996 3002.4249~Y~0~0~0.4~"))


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

(module+ test
  (lcsc->uuid "C440457"))

(define (lcsc->fp lcsc-id)
  (uuid->fp (lcsc->uuid lcsc-id)))

;; "USB-C-SMD_TYPE-C16PIN_2020-07-28_23-37-02.json"
(define (uuid->fp uuid)
  "Cached version of getting easyeda file."
  ;; 1. check cache to see if uuid already built
  ;; 2. check if we have record of which file the uuid is from
  ;; 1. get fname from uuid
  ;; 2. if fname exist locally, directly
  ;;
  ;; https://easyeda.com/api/components/3e67c3e3b97b4ff38f0c80567ed48498
  (let ([fname (build-path (bhdl-footprints-path) "easyeda" (~a uuid ".json"))])
    (when (not (file-exists? fname))
      (let* ([url (~a "https://easyeda.com/api/components/" uuid)]
             [_ (displayln (~a "requesting " url " .."))]
             [obj (call/input-url (string->url url)
                                  get-pure-port
                                  (lambda (in)
                                    (hash-ref-ref (read-json in)
                                                  'result
                                                  ;; FIXME seems to be inconsistent, some json doesn't have this
                                                  'packageDetail
                                                  'dataStr)))])
        (call-with-output-file fname
          (lambda (out) (write-json obj out)))
        ;; TODO pretty print using python
        ;; (system (~a "python -m json.tool " fname))
        ))
    ;; at this point, the file should appear locally
    (read-easyeda fname)))

(module+ test
  (uuid->fp (lcsc->uuid "C440457"))
  (uuid->fp (lcsc->uuid "C393939"))
  (uuid->fp "C456012"))


;; kailh socket with different spacing units
;; 1, 1.25, 1.5, 1.75, 2.25, 6.25
;; Since I don't have the footprints, how can I make those?
;; I could probably use some HACK like get the width of 1u fp and add some spacing
;; But I cannot generate gerber for that fp. I should add some lines to fp itself.
;; The origin is 0,0, I should:
;; - get the positions of all lines
;; - get the left most and right most
;; - compute the unit
;; - compute the new ..

(define kailh-socket-fp-1 (uuid->fp "bd8c6c64dc7b4d18806bb8859f9f2606"))

(define (fp-kailh-socket [unit 1])
  (match-let* ([(list x1 y1 x2 y2) (get-4-corners (footprint-lines kailh-socket-fp-1))]
              [u1 (- x2 x1)]
              [Δx (/ (* (- unit 1) u1) 2)]
              [stroke (line-spec-width (car (footprint-lines kailh-socket-fp-1)))]
              [(list x1n x2n) (list (- x1 Δx) (+ x2 Δx))])
             ;; more lines
            (footprint (append (footprint-lines kailh-socket-fp-1)
                               (list (line-spec x1n y1 x2n y1 stroke)
                                   (line-spec x2n y1 x2n y2 stroke)
                                   (line-spec x2n y2 x1n y2 stroke)
                                   (line-spec x1n y2 x1n y1 stroke)))
                       (footprint-pads kailh-socket-fp-1)
                       ;; not place at the middle, but bottom right
                       (list (text-spec (+ x1 (* u1 0.75))
                                        (+ y1 (* (- y2 y1) 0.75))))
                       (footprint-holes kailh-socket-fp-1))
             ))

