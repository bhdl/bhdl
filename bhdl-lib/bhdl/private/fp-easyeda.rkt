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
         json)

(provide fp-SKRPACE010
         fp-esp32-wrover-e
         fp-usb-c-16)

(define (read-easycad fname)
  "Read EasyCAD json file."
  (let ([jobj (call-with-input-file fname
                (lambda (in) (read-json in)))])
    (match-let* ([origin-x (10mil->mm (hash-ref-ref jobj 'head 'x))]
                 [origin-y (10mil->mm (hash-ref-ref jobj 'head 'y))]
                 [pre (hash-ref-ref jobj 'head 'c_para 'pre)]
                 [canvas (hash-ref jobj 'canvas)]
                 [shapes (hash-ref jobj 'shape)]
                 [tracks (filter (lambda (x) (string-prefix? x "TRACK")) shapes)]
                 [pads (filter (lambda (x) (string-prefix? x "PAD")) shapes)]
                 [(hash-table ('x x) ('y y) ('width width) ('height height))
                  (hash-ref jobj 'BBox)])
      ;; FIXME unit
      (let* ([line-specs (flatten (for/list ([track tracks])
                                    (parse-track track)))]
             [pad-specs (for/list ([pad pads])
                          (parse-pad pad))]
             [fn (lambda (item) (spec-offset item origin-x origin-y))])
        (footprint (map fn line-specs)
                   (map fn pad-specs))))))

(define (spec-offset spec offx offy)
  (match spec
    [(line-spec x1 y1 x2 y2 width)
     (line-spec (- x1 offx) (- y1 offy) (- x2 offx) (- y2 offy) width)]
    [(pad-spec num x y mounting-type shape size dsize)
     (pad-spec num (- x offx) (- y offy) mounting-type shape size dsize)]
    [else (error "spec-offset")]))

(define (10mil->mm x)
  (* 25.4 (/ (* x 10) 1000)))

(define (parse-track str)
  (match-let ([(list _ stroke layer net points ID
                     ;; the last field seems to be optional, HACK using ID
                     ;; ... to handle the optional value
                     ...)
               (string-split str "~")])
    (let ([points (group-by-2 (string-split points))])
      (for/list ([a points]
                 [b (rest points)])
        (line-spec (10mil->mm (string->number (first a)))
                   (10mil->mm (string->number (second a)))
                   (10mil->mm (string->number (first b)))
                   (10mil->mm (string->number (second b)))
                   (10mil->mm (string->number stroke)))))))

(define (parse-pad str)
  (match-let ([(list _ shape x y
                     ;; seems to be in reverse order???
                     height width
                     layer net number
                     hole-radius points rotation ID hole-length hole-points _ _ _ _ _)
               (string-split str "~")])
    (pad-spec number
              (10mil->mm (string->number x))
              (10mil->mm (string->number y))
              ;; FIXME fixed smd
              'smd
              ;; FIXME shape corresponding to kicad
              (string->symbol (string-downcase shape))
              (list (10mil->mm (string->number width))
                    (10mil->mm (string->number height)))
              ;; hole? shape?
              (10mil->mm (string->number hole-radius)))))

(module+ test
  (parse-track "TRACK~1~3~S$216~4035.4331 2925.5906 3964.5669 2925.5906~gge219~0")
  (parse-track "TRACK~1~3~S$222~3970 2950 4010 2950 4010 2970 4030 2970 4030 3045 3970 3045 3970 2950~gge221~0")
  (parse-track "TRACK~1~3~S$39~3998 2996 4002 2996~gge38~")
  (parse-pad "PAD~RECT~3964.567~2955~3.5433~7.874~1~~1~0~3960.6299 2956.7717 3960.6299 2953.2283 3968.5039 2953.2283 3968.5039 2956.7717~90~gge5~0~~Y~0~~~3964.567,2955"))


(define fp-esp32-wrover-e
  ;; FIXME this is S2
  ;; (kicad-helper "RF_Module.pretty/ESP32-S2-WROVER.kicad_mod")
  ;;
  ;; TODO remove hard-coded path
  (read-easycad (expand-user-path "~/git/bhdl/bhdl-lib/bhdl/easyeda/WIFIM-SMD_ESP32-WROVER_2020-07-23_13-18-22.json")))

(define fp-SKRPACE010
  (read-easycad (expand-user-path "~/git/bhdl/bhdl-lib/bhdl/easyeda/KEY-SMD_4P-L4.2-W3.2-P2.20-LS4.6_2020-07-28_23-08-45.json")))

(module+ test
  (define fname "easyeda/WIFIM-SMD_ESP32-WROVER_2020-07-23_13-18-22.json")
  (define jobj (call-with-input-file fname
                 (lambda (in) (read-json in))))
  (read-easycad fname))


(define fp-usb-c-16
  (read-easycad (expand-user-path "~/git/bhdl/bhdl-lib/bhdl/easyeda/USB-C-SMD_TYPE-C16PIN_2020-07-28_23-37-02.json")))

