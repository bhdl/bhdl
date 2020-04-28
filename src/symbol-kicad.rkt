#lang racket

;; read kicad symbol libraries

(require (for-syntax syntax/parse
                     racket/string)
         megaparsack
         megaparsack/text

         data/monad
         data/applicative

         ;; parsack

         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)

         syntax/parse/define
         rackunit
         (except-in pict blank))

#;
(module+ test
  (parse-result $letter "abc")

  (parse-result (parser-compose (char #\[)
                                (x <- $letter)
                                (y <- $letter)
                                (char #\])
                                (return (list x y)))
                "[ab]")
  (define (mySepBy1 p sep) (parser-cons p (many (try (>> sep p)))))
  (define (mySepBy p sep) (<or> (mySepBy1 p sep) 
                                (return null)))
  (parse-result (parser-compose (mySepBy (char #\a) (char #\.))
                                (char #\.))
                "a.a.a.")

  )

;; configuration
(define kicad-symbol-path
  (make-parameter (expand-user-path "~/git/reading/kicad-symbols/")))

#;
(define (read-kicad-lib fname)
  "Read a kicad mod file, parse it, and return a symbol object."
  (call-with-input-file fname
    (λ (in)
      ())))

(define alphanum/p (or/p letter/p digit/p (char/p #\-)))
(define identifier/p (do [x <- (many/p alphanum/p)]
                         (pure (apply string x))))
(define just-space/p (char/p #\space))
(define not-newline/p (char-not/p #\newline))
(define sep/p (many/p space/p #:min 1))
(define skip-to-newline/p
  (do (many/p not-newline/p)
      ;; and consume all following newlines and whitespaces
      (many/p (or/p (char/p #\newline)
                    space/p))))

(define number-no-sign/p (or/p
                          ;; FIXME this is not clean: 1. I have to use try/p for
                          ;; explicit backtracknig, 2. the order matters; 3. the
                          ;; second choice is problematic, e.g. it will match
                          ;; 123,456
                          (try/p
                           (do [a <- integer/p]
                               (char/p #\.)
                             [b <- integer/p]
                             (pure (+ a (/ b (expt 10 (ceiling (log b 10))))))))
                          integer/p))

(define number/p (or/p (do (char/p #\+) number-no-sign/p)
                       (do (char/p #\-)
                           [value <- number-no-sign/p]
                         (pure (- value)))
                       number-no-sign/p))

(define ALIAS/p (do (string/p "ALIAS")
                    ;; FIXME maybe many aliases
                    [id <- identifier/p]
                  skip-to-newline/p
                  (pure id)))

(define FPLIST-pattern/p (do [res <- (many/p (or/p letter/p
                                                   digit/p
                                                   (char/p #\*)
                                                   (char/p #\.)) #:min 1)]
                             (pure (apply string res))))
(define FPLIST/p (do (string/p "$FPLIST")
                     sep/p
                   ;; [patterns <- FPLIST-pattern/p]
                   [patterns <- (many/p FPLIST-pattern/p
                                        #:sep sep/p)]
                   sep/p
                   (string/p "$ENDFPLIST")
                   skip-to-newline/p
                   (pure patterns)))

(module+ test
  (parse-string (do (string/p "$FPLIST")
                    (many/p identifier/p #:sep sep/p)
                  skip-to-newline/p
                  (string/p "$ENDFPLIST"))
                "$FPLIST hello world $ENDFPLIST")
  (parse-string FPLIST-pattern/p "DFN*8*1EP*2x3mm*P0.5mm*")
  ;; https://github.com/lexi-lambda/megaparsack/issues/27 thus basically I
  ;; cannot solve it.
  ;;
  ;; UPDATE: I fixed it: https://github.com/lexi-lambda/megaparsack/pull/28
  ;;
  ;; inspired by
  ;; https://stackoverflow.com/questions/60222934/avoid-parsing-last-separator-with-sepby
  (parse-result! (parse-string (do (many/p (char/p #\a)
                                           #:sep (char/p #\.))
                                   (char/p #\.))
                               "a.a.a."))
  (parse-result!
   (parse-string FPLIST/p "$FPLIST
DFN*8*1EP*2x3mm*P0.5mm*
DFN*8*1EP*2x3mm*P0.5mm*
$ENDFPLIST")))

(struct kicad-symbol
  (name fplist
        ;; FIXME how many rects are possible?
        rect
        pins)
  #:prefab)
(struct draw-rect
  (x1 y1 x2 y2)
  #:prefab)
(struct draw-pin
  (name index xoff yoff)
  #:prefab)

(define S-line/p (do (string/p "S")
                     ;; FIXME remove these many/p
                     sep/p
                   [x1 <- number/p]
                   sep/p
                   [y1 <- number/p]
                   sep/p
                   [x2 <- number/p]
                   sep/p
                   [y2 <- number/p]
                   skip-to-newline/p
                   (pure (draw-rect x1 y1 x2 y2))))
(define X-line/p (do (string/p "X")
                     sep/p
                   [name <- identifier/p]
                   sep/p
                   [index <- number/p]
                   sep/p
                   [xoff <- number/p]
                   sep/p
                   [yoff <- number/p]
                   skip-to-newline/p
                   (pure (draw-pin name index xoff yoff))))

(module+ test
  (parse-string number/p "-123.34")
  (parse-string number-no-sign/p "123.456")
  (parse-string number-no-sign/p "123"))

(define DRAW-item/p (or/p S-line/p
                          X-line/p))

(define DRAW/p (do (string/p "DRAW")
                   skip-to-newline/p
                 [items <- (many/p DRAW-item/p)]
                 (string/p "ENDDRAW")
                 skip-to-newline/p
                 (pure items)))


(define quoted-string/p (do (char/p #\")
                            [res <- (many/p (char-not/p #\"))]
                          (char/p #\")
                          (pure res)))

;; this is symbol, e.g. U for IC, R for resistor
(define F0/p (do (string/p "F0")
                 sep/p
               [symbol <- quoted-string/p]
               skip-to-newline/p
               (pure symbol)))

;; name to draw with the symbol, unimportant
(define F1/p (do (string/p "F1")
                 skip-to-newline/p))

;; footprint name, FIXME is this the default one?
(define F2/p (do (string/p "F2")
                 skip-to-newline/p))

;; datasheet path, not important
(define F3/p (do (string/p "F3")
                 skip-to-newline/p))

(define DEF-line/p (do (string/p "DEF")
                       sep/p
                     ;; PIC10F200-IMC
                     [name <- identifier/p]
                     skip-to-newline/p
                     (pure name)))

(define DEF/p (do [name <- DEF-line/p]
                  F0/p
                F1/p
                F2/p
                F3/p
                ;; alias is not important
                [alias <- ALIAS/p]
                ;; Footprint patterns that are valid. This should be important
                [fplist <- FPLIST/p]
                [draw <- DRAW/p]
                (string/p "ENDDEF")
                (pure (kicad-symbol name fplist
                                    (filter draw-rect? draw)
                                    (filter draw-pin? draw)))))


(module+ test
  (parse-string (many/p not-newline/p) "he\nnnmnfd")
  (parse-string (do (many/p not-newline/p)
                    ;; (char/p #\newline)
                  (many/p not-newline/p #:min 1)) "hello
world")
  (parse-result! (parse-string DEF/p "DEF PIC10F200-IMC"))
  (apply string '(#\P #\I #\C #\1 #\0 #\F #\2 #\0 #\0 #\- #\I #\M #\C))
  (parse-string (many/p (char/p #\a)) "aaaa")
  (parse-string identifier/p "hello")
  (parse-string (string/p "true") "true")
  
  (parse-result! (parse-string DRAW/p "DRAW
S -400 400 400 -400 0 1 10 f
X VDD 2 0 600 200 D 50 50 1 1 W
X GP3 8 600 100 200 L 50 50 1 1 I
ENDDRAW"))
  (parse-string DRAW-item/p "X VDD 2 0 600 200 D 50 50 1 1 W")
  (parse-string DRAW-item/p "S -400 400 400 -400 0 1 10 f")
  (parse-string S-line/p "S -400 400 400 -400 0 1 10 f")
  (parse-string (many/p DRAW-item/p)
                "S -400 400 400 -400 0 1 10 f
X VDD 2 0 600 200 D 50 50 1 1 W
X GP3 8 600 100 200 L 50 50 1 1 I")
  (parse-string X-line/p "X VDD 2 0 600 200 D 50 50 1 1 W")
  (parse-result! (parse-string DEF/p "DEF PIC10F200-IMC U 0 40 Y Y 1 F N
F0 \"U\" 50 550 50 H V L CNN
F1 \"PIC10F200-IMC\" 50 450 50 H V L CNN
F2 \"Package_DFN_QFN:DFN-8-1EP_2x3mm_P0.5mm_EP0.61x2.2mm\" 50 650 50 H I L CIN
F3 \"\" 0 0 50 H I C CNN
ALIAS PIC10F202-IMC
$FPLIST
 DFN*8*1EP*2x3mm*P0.5mm*
$ENDFPLIST
DRAW
S -400 400 400 -400 0 1 10 f
X VDD 2 0 600 200 D 50 50 1 1 W
X GP2 3 600 -100 200 L 50 50 1 1 B
X GP1 4 -600 -100 200 R 50 50 1 1 B
X GP0 5 -600 100 200 R 50 50 1 1 B
X VSS 7 0 -600 200 U 50 50 1 1 W
X GP3 8 600 100 200 L 50 50 1 1 I
ENDDRAW
ENDDEF")))

(define (parse-kicad-lib-file fname)
  (parse-string DEF/p (call-with-input-file fname
                        (λ (in) (port->string in)))))


;; FIXME well, I still need a lexer to do preprocessing, for:
;; 1. comments
;; 2. ignoring spaces and condensing newlines
;; However, using lexers and tokens creates other problems:
;; 1. I'll need to define different tokens
;; 2. it is impossible to do parser combinator inside token
(module+ test
  (parse-kicad-lib-file "/home/hebi/git/reading/kicad-symbols/4xxx.lib"))

