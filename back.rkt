(define symbol-74469
  (make-sch-symbol (outline (rectangle 600 1400))
                   (pin 1 "CLK" -300 -300)
                   (pin 2 "LD" -300 -400)
                   (pin 3 "D0" -300 600)
                   (pin 4 "D1" -300 500)
                   (pin 5 "D2" -300 400)
                   (pin 6 "D3" -300 300)
                   (pin 7 "D4" -300 200)
                   (pin 8 "D5" -300 100)
                   (pin 9 "D6" -300 0)
                   (pin 10 "D7" -300 -100)
                   (pin 11 "UD" -300 -500)
                   (pin 12 "GND" 0 -700)
                   (pin 13 "OE" 300 -300)
                   (pin 14 "CBO" 300 -400)
                   (pin 15 "Q7" 300 -100)
                   (pin 16 "Q6" 300 0)
                   (pin 17 "Q5" 300 100)
                   (pin 18 "Q4" 300 200)
                   (pin 19 "Q3" 300 300)
                   (pin 20 "Q2" 300 400)
                   (pin 21 "Q1" 300 500)
                   (pin 22 "Q0" 300 600)
                   (pin 23 "CBI" -300 -600)
                   (pin 24 "VCC" 0 700)))
(define p (let* ([outline (sch-symbol-outline symbol-74469)]
                 ;; TODO assuming rectangle
                 [w (second outline)]
                 [h (third outline)])
            (define base (filled-rectangle w h
                                           #:color "Khaki"
                                           #:border-color "Brown"
                                           #:border-width 10))
            ;; for all pins
            (for/fold ([res base])
                      ([pin (sch-symbol-pins symbol-74469)])
              (pin-over
               res
               (+ (sch-symbol-pin-x pin) 300)
               (- 1400 (+ (sch-symbol-pin-y pin) 700))
               (text (sch-symbol-pin-name pin) 'default 50)))))
(struct sch-symbol
  (pins
   children
   connections)
  #:prefab)

(struct sch-symbol-pin
  (name x y)
  #:prefab)

(define-syntax (make-sch-symbol stx)
  (syntax-parse stx
    [(_ (outline out)
        (pin num name x y) ...)
     #'(sch-symbol 'out (list (sch-symbol-pin num name x y) ...))]))

(define-syntax (make-rect-symbol stx)
  (syntax-parse stx
    [(_ (left l ...)
        (right r ...)
        (top t ...)
        (down d ...))
     
     #'(list (rect-IC->pict #'(rect-IC-symbol '(l ...)
                                              '(r ...)
                                              '(t ...)
                                              '(d ...)))
             (list l ... r ... t ... d ...))]))


