
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPRMkQVGwhBw/CPadywggP7TT racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide bhdl-footprints-path download-footprints merge-fp get-corner get-4-corners
    (struct-out footprint)
(struct-out line-spec)
(struct-out pad-spec)
(struct-out text-spec)
    
    )

    (require racket/system)

(module+ test
     (current-directory)
(directory-list "/mount/shared")
; (system "echo 'hallo'")
; (system "git")
; (system "git clone --recursive https://github.com/lihebi/bhdl-footprints /mount/shared/bhdl-footprints")
(system "ls /mount/shared/")
    )

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)


(struct footprint
  ;; line will have start (x,y), end (x,y), width
  ;; pads will have num, mounting-type, (shape attr), (x y)
  (lines
   pads
   texts
   holes)
  #:prefab)

(struct line-spec
  (x1 y1 x2 y2 width)
  #:prefab)

(struct pad-spec
  (name
   x y a
   mounting-type
   shape
   size
   dsize
   ;; possible values: top, bottom, multi
   layer)
  #:prefab)

(struct text-spec
        (x y))


(define bhdl-footprints-path
  ;; FIXME make this configurable
  (make-parameter
    (expand-user-path
      ;  "/mount/shared/bhdl-footprints"
      (build-path 
        (case (system-type)
          [(unix) "~/.config/bhdl"]
          [(macosx) "~/Library/Application Support/bhdl"]
          [(windows) 
           "C:\\Users\\%USER%\\AppData\\Roaming\\bhdl"]
           [else (error "System type error")])
        "bhdl-footprints")
      )))

(define (download-footprints)
  (let ([dir (bhdl-footprints-path)])
    (when (not (directory-exists? dir))
      (displayln (~a "Downloading footprints to " dir))
      (parameterize 
        ([current-directory (build-path dir "..")])
        (system (~a "git clone --recursive "
                    "https://github.com/lihebi/bhdl-footprints "
                    ))))))

;; warning downloading
(download-footprints)


(define (merge-fp fp1 fp2)
  (match-let ([(footprint lines1 pads1 texts1 holes1) fp1]
              [(footprint lines2 pads2 texts2 holes2) fp2])
             (footprint (append lines1 lines2)
                        (append pads1 pads2)
                        ;; actually I'm using the first texts
                        (append texts1 texts2)
                        (append holes1 holes2))))

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
  )
    