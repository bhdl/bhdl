#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

;; draw a given gerber file onto dc%


(define (gbr->instructions gbr-file)
  (filter
   identity
   (for/list ([line (port->lines (open-input-file gbr-file))])
     (match line
       [(regexp #rx"^G04 (.*)$" (list _ a)) #f]
       [(pregexp
         "%FSLAX(\\d)(\\d)Y(\\d)(\\d)\\*%"
         (list _ x1 x2 y1 y2)) (list 'FS
                                     (string->number x1)
                                     (string->number x2)
                                     (string->number y1)
                                     (string->number y2))]
       [(regexp "^%MO(..)\\*%$" (list _ m)) (list 'MO m)]
       [(regexp "^%LP(.)\\*%$" (list _ m)) (list 'LP m)]
       [(pregexp "^%ADD(\\d+)(\\w),(.*)\\*%$" (list _ dcode shape attr))
        (list 'AD (string->number dcode) shape (map string->number (string-split attr "X")))]
       [(pregexp "^D(\\d+)\\*$" (list _ dcode))
        (list 'D (string->number dcode))]
       [(pregexp (~a "^X(-?\\d+)Y(-?\\d+)"
                     "(?:I(-?\\d+))?(?:J(-?\\d+))"
                     "?D(\\d+)\\*$")
                 (list _ x y i j d))
        (list 'XY
              (string->number x)
              (string->number y)
              (if i (string->number i) i)
              (if j (string->number j) j)
              (string->number d))]
       [(regexp "^M02\\*$") (list 'M02)]
       ))))

(define (execute-gbr-instructions instructions)
  (define FS-x #f)
  (define FS-y #f)
  (define unit #f)
  (define cur-LP #f)
  (define cur-aperture #f)
  (define-values (add-aperture set-aperture)
    (let ([aperture '()])
      (values (λ (dcode shape attr)
                (set! aperture (cons (list dcode shape attr) aperture)))
              (λ (dcode)
                (match (assoc dcode aperture)
                  [(list _ shape attr)
                   (set! cur-aperture (list shape attr))])))))


  ;; global setting and aperture registering
  (for ([instruction instructions])
    (match instruction
      [(list 'FS _ x _ y) (set! FS-x x)
                          (set! FS-y y)]
      [(list 'MO u) (case u
                      [("MM") (set! unit 'mm)]
                      [("IN") (set! unit 'inch)])]
      [(list 'AD dcode shape (list attr ...))
       (add-aperture dcode shape attr)]
      [else #f]))

  ;; I'll need to known the size of the plot to create a dc%
  (define-values (xmax xmin ymax ymin)
    (let ([res (filter
                identity
                (for/list ([instruction instructions])
                  (match instruction
                    [(list 'XY x y i j d)
                     (list (/ x (expt 10 FS-x))
                           (/ y (expt 10 FS-y)))]
                    [else #f])))])
      (values (apply max (map first res))
              (apply min (map first res))
              (apply max (map second res))
              (apply min (map second res)))))

  (define (xx x)
    (- (/ x (expt 10 FS-x)) xmin))
  (define (yy y)
    (- (/ y (expt 10 FS-y)) ymin))

  (define (flash-rectangle dc x y)
    (let-values ([(dx dy) (values (first (second cur-aperture))
                                  (second (second cur-aperture)))])
      ;; (println (~a x y dx dy #:separator " "))
      
      (send dc draw-rectangle
            (- x (/ dx 2))
            (- y (/ dy 2))
            dx
            dy)))

  (define (draw dc dx dy)
    (define cur-pos #f)
    ;; the default pen is sooooo big
    (send dc set-pen "black" 0.05 'solid)
    ;; for pads
    (send dc set-brush "red" 'solid)
    (for ([instruction instructions])
      (match instruction
        [(list 'LP d) (case d
                        [("D") (set! cur-LP 'dark)]
                        [("C") (set! cur-LP 'clear)])]
        [(list 'D dcode)
         (set-aperture dcode)]
        [(list 'XY x y i j d)
         (case d
           ;; TODO actually draw
           [(1)
            (send dc draw-line
                  (car cur-pos) (cdr cur-pos)
                  (xx x) (yy y))
            #f]
           [(2) #f]
           ;; flash current aperture
           [(3)
            (case (first cur-aperture)
              [("R") (flash-rectangle dc (xx x) (yy y))]
              [("O") (send dc draw-ellipse (xx x) (yy y)
                           (first (second cur-aperture))
                           (second (second cur-aperture)))])
            #f])
         (set! cur-pos (cons (xx x) (yy y)))]
        ;; TODO check if this instruction is present at the last
        [(list 'M02) "end"]
        [else #f])))
  
  ;; return dc as a pict
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (draw dc dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
      (* (- xmax xmin) 1.05)
      (* (- ymax ymin) 1.05)))

(define (view-gerber gbr-file)
  "Return a pict for the gerber file."
  (scale (execute-gbr-instructions (gbr->instructions gbr-file)) 30))

(module+ test
  (view-gerber "out.gbr")
  )

