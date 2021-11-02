
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPRMkQVGwhBw/CPVMzLibHe9N racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPRMkQVGwhBw/CPadywggP7TT/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide gerber-file->pict+offset gerber-file->pict
    
    
    )

    (require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         racket/match
         rackunit
         pict
         racket/draw)

(define (gbr->instructions gbr-file)
  (filter
   identity
   (for/list ([line (filter non-empty-string? (port->lines (open-input-file gbr-file)))])
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
  ;; FIXME define aperture data structure
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
  
  (define (xx x)
    (/ x (expt 10 FS-x)))
  (define (yy y)
    (/ y (expt 10 FS-y)))

  (define (flash-rectangle dc x y)
    (let-values ([(dx dy) (values (first (second cur-aperture))
                                  (second (second cur-aperture)))])
      ;; (println (~a x y dx dy #:separator " "))
      (send dc draw-rectangle
            (- x (/ dx 2))
            (- y (/ dy 2))
            dx
            dy)
      ;; update bounding box
      (update-bounding-box x y (/ dx 2) (/ dy 2))))

  (define (flash-ellipse dc x y)
    (let-values ([(dx dy) (values (first (second cur-aperture))
                                  (second (second cur-aperture)))])
      (send dc draw-ellipse
            (- x (/ dx 2))
            (- y (/ dy 2))
            dx
            dy)
      (update-bounding-box x y (/ dx 2) (/ dy 2))))

  (define-values (update-bounding-box get-bounding-box)
    (let-values ([(xmax xmin ymax ymin) (values 0 0 0 0)])
      (values (λ (x y (dx 0) (dy 0))
                (set! xmax (max (+ x dx) xmax))
                (set! xmin (min (- x dx) xmin))
                (set! ymax (max (+ y dy) ymax))
                (set! ymin (min (- y dy) ymin)))
              (λ ()
                (values xmax xmin ymax ymin)))))

  ;; FIXME This draw function is called everytime the picture is
  ;; rendered
  ;;
  (define (draw dc)
    (define cur-pos #f)
    ;; the default pen is sooooo big
    ;; FIXME fixed line width 0.05
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
         (unless cur-aperture
           (error "cur aperture not set"))
         (case d
           ;; Compute min max of x,y during the draw, because I need
           ;; to access the current aperture
           ;;
           ;; FIXME magic number 0.05 repeated
           [(1) (update-bounding-box (xx x) (yy y))
                (send dc draw-line
                      (car cur-pos) (cdr cur-pos)
                      (xx x) (yy y))]
           [(2) (update-bounding-box (xx x) (yy y))]
           ;; flash current aperture
           [(3) (let-values ([(dx dy) (values (first (second cur-aperture))
                                              (second (second cur-aperture)))])
                  ;; I should consider the width and height of the pad. This is
                  ;; done in the flash function.
                  (update-bounding-box (xx x) (yy y)))
                (case (first cur-aperture)
                  [("R") (flash-rectangle dc (xx x) (yy y))]
                  ;; FIXME I should flash the ellipse
                  [("O") (flash-ellipse dc (xx x) (yy y))])])
         (set! cur-pos (cons (xx x) (yy y)))]
        ;; TODO check if this instruction is present at the last
        [(list 'M02) "end"]
        [else #f])))
  
  ;; return dc as a pict
  (define res (dc (λ (dc dx dy)
                    (define old-brush (send dc get-brush))
                    (define old-pen (send dc get-pen))
                    (define-values (old-x old-y) (send dc get-origin))
                    (send dc set-origin dx dy)
                    
                    (draw dc)
                    
                    (send dc set-brush old-brush)
                    (send dc set-pen old-pen)
                    (send dc set-origin old-x old-y))
                  0 0))

  ;; FIXME get-bounding-box should be called AFTER the drawing
  (define-values (xmax xmin ymax ymin) (get-bounding-box))

  ;; (println (list xmax xmin ymax ymin))
  (values
   (inset res
          (- 0 xmin) (- 0 ymin) xmax ymax)
   (list xmin ymin xmax ymax)))

(define (gerber-file->pict+offset gbr-file)
  "Return a pict for the gerber file."
  (let-values ([(p box) (execute-gbr-instructions (gbr->instructions gbr-file))])
    (match box
      [(list xmin ymin _ _) (values p (Point xmin ymin 0))])))

(define (gerber-file->pict gbr-file)
  (let-values ([(p _) (gerber-file->pict+offset gbr-file)])
    p))
  )
    