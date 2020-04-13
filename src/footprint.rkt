#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide (struct-out footprint)
         (struct-out line-spec)
         (struct-out pad-spec)
         read-kicad-mod
         footprint-get-pad-loc)

(struct footprint
  ;; line will have start (x,y), end (x,y), width
  ;; pads will have num, mounting-type, (shape attr), (x y)
  (lines
   pads))
(struct line-spec
  (x1 y1 x2 y2 width))
(struct pad-spec
  (x y num mounting-type shape shape-attr))

(define (footprint-get-pad-loc fp num)
  (let ([pad (first (filter (λ (x)
                              (= (pad-spec-num x) num))
                            (footprint-pads fp)))])
    (values (pad-spec-x pad)
            (pad-spec-y pad))))

(define (read-kicad-mod fname)
  "Read a kicad mod file, parse it, and return a footprint object."
  (let ([kicad-mod (let ([in (open-input-file fname)])
                     (begin0
                         (read in)
                       (close-input-port in)))])
    (let ([specs (match kicad-mod
                   [(list 'module name layer body ...)
                    (filter
                     identity
                     (for/list [(e body)]
                       (match e
                         ;; TODO
                         ;; FIXME optional z
                         [`(fp_text ,_ ,text (at ,x ,y ,z ...) (layer ,l) ,other ...)
                          #f]
                         [`(fp_arc (start ,sx ,sy) (end ,ex ,ey) (angle ,ag) (layer ,l) (width ,w))
                          #f]
                         [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                          (line-spec sx sy ex ey w)]
                         ;; FIXME optional z
                         [`(pad ,num ,mounting-type ,shape (at ,x ,y ,z ...)
                                (size ,s1 ,s2) ,other-attrs ...)
                          (pad-spec x y num mounting-type shape `((size ,s1 ,s2)))]
                         ;; TODO
                         [`(fp_circle ,other ...)
                          #f]
                         [`(tedit ,other ...) #f]
                         [`(descr ,other ...) #f]
                         [`(tags ,other ...) #f]
                         [`(model ,other ...) #f]
                         [`(attr ,other ...) #f])))])])
      (let ([line-specs (filter line-spec? specs)]
            [pad-specs (filter pad-spec? specs)])
        (footprint line-specs pad-specs)))))

