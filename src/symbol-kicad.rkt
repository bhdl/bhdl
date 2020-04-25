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
(define skip-to-newline/p
  (do (many/p not-newline/p)
      ;; and consume all following newlines and whitespaces
      (many/p (or/p (char/p #\newline)
                    space/p))))

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
                     (many/p space/p #:min 1)
                   ;; [patterns <- FPLIST-pattern/p]
                   [patterns <- (many/p FPLIST-pattern/p
                                        #:sep (many/p space/p #:min 1))]
                   (many/p space/p #:min 1)
                   (string/p "$ENDFPLIST")
                   (pure patterns)))

(module+ test
  (parse-string (do (string/p "$FPLIST")
                    (many/p identifier/p #:sep (many/p space/p #:min 1))
                    (many/p space/p)
                    (string/p "$ENDFPLIST"))
                "$FPLIST hello world $ENDFPLIST")
  (parse-string FPLIST-pattern/p "DFN*8*1EP*2x3mm*P0.5mm*")
  (parse-string (do
                    ;; (try/p (many/p FPLIST-pattern/p #:sep (many/p space/p #:min 1)))
                  ;;   FPLIST-pattern/p
                  ;;   (many/p space/p #:min 1)
                  ;; FPLIST-pattern/p
                    (many/p FPLIST-pattern/p #:sep (many/p space/p #:min 1))
                    (many/p space/p)
                  ;; (string/p "%")
                  ;; (string/p "$END")
                  )
                "DFN*8*1EP*2x3mm*P0.5mm*  DFN*8*1EP*2x3mm*P0.5mm* ")
  ;; https://github.com/lexi-lambda/megaparsack/issues/27 thus basically I
  ;; cannot solve it.
  ;;
  ;; UPDATE: I fixed it: https://github.com/lexi-lambda/megaparsack/pull/28
  ;;
  ;; inspired by
  ;; https://stackoverflow.com/questions/60222934/avoid-parsing-last-separator-with-sepby
  (parse-result! (parse-string (do (my-many/p (char/p #\a)
                                              #:sep (char/p #\.))
                                   (char/p #\.))
                               "a.a.a."))
  (define (my-many/p p
                  #:sep [sep void/p]
                  #:min [min-count 0]
                  #:max [max-count +inf.0])
    (define (loop-mandatory p
                            #:min [min-count min-count]
                            #:max [max-count max-count]
                            #:recur-parser [recur p])
      (cond [(zero? min-count) (loop-optional p max-count #:recur-parser recur)]
            [else
             (define rest/p
               (loop-mandatory recur
                               #:min (sub1 min-count)
                               #:max (sub1 max-count)))
             ((pure cons) p rest/p)]))
    (define (loop-optional p max-count #:recur-parser [recur p])
      (if (zero? max-count)
          (pure '())
          (or/p (lazy/p ((pure cons) p (loop-optional recur (sub1 max-count))))
                (pure '()))))
    (loop-mandatory p #:recur-parser (try/p (do sep p))))
  (parse-result!
   (parse-string FPLIST/p "$FPLIST
DFN*8*1EP*2x3mm*P0.5mm*
DFN*8*1EP*2x3mm*P0.5mm*
$ENDFPLIST"))

  (parse-result! (parse-string (many/p FPLIST-pattern/p #:sep (many/p space/p))
                               "DFN*8*1EP*2x3mm*P0.5mm*   DFN*8*1EP*2x3mm*P0.5mm*"))
  (parse-string (many/p identifier/p #:sep space/p) "a a* aa-fdj"))

(define S-line/p (do (string/p "S")
                     [x1 <- integer/p]
                   [y1 <- integer/p]
                   [x2 <- integer/p]
                   [y2 <- integer/p]
                   skip-to-newline/p
                   (pure x1 y1 x2 y2)))
(define X-line/p (do (string/p "X")
                     [name <- identifier/p]
                   [pin <- integer/p]
                   [x <- integer/p]
                   [y <- integer/p]
                   skip-to-newline/p
                   (pure (list name pin x y))))
(define DRAW-item/p (or/p S-line/p
                          X-line/p))

(define DRAW/p (do (string/p "DRAW")
                   [items <- (many/p DRAW-item/p)]
                 (string/p "ENDDRAW")
                 (pure items)))

(define quoted-string/p (do (char/p #\")
                            [res <- (many/p (char-not/p #\"))]
                          (char/p #\")
                          (pure res)))
(define F0/p (do (string/p "F0")
                 (many/p space/p #:min 1)
               ;; this is symbol, e.g. U for IC, R for resistor
               [symbol <- quoted-string/p]
               skip-to-newline/p
               (pure symbol)))
(define F1/p (do (string/p "F1")
                 skip-to-newline/p))
(define F2/p (do (string/p "F2")
                 skip-to-newline/p))
(define F3/p (do (string/p "F3")
                 skip-to-newline/p))

(define DEF-line/p (do (string/p "DEF")
                       (many/p space/p #:min 1)
                     ;; PIC10F200-IMC
                     [name <- identifier/p]
                     skip-to-newline/p
                     (pure name)))

(define DEF/p (do [name <- DEF-line/p]
                  F0/p
                F1/p
                F2/p
                F3/p
                [alias <- ALIAS/p]
                [fplist <- FPLIST/p]
                [draw <- DRAW/p]
                (string/p "ENDDEF")
                (pure (list name alias fplist draw))))



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
  (define def-str "DEF PIC10F200-IMC U 0 40 Y Y 1 F N
F0 \"U\" 50 550 50 H V L CNN
F1 \"PIC10F200-IMC\" 50 450 50 H V L CNN
F2 \"Package_DFN_QFN:DFN-8-1EP_2x3mm_P0.5mm_EP0.61x2.2mm\" 50 650 50 H I L CIN
F3 \"\" 0 0 50 H I C CNN
ALIAS PIC10F202-IMC
$FPLIST
 DFN*8*1EP*2x3mm*P0.5mm*
$ENDFPLIST
ENDDEF")
  (parse-result! (parse-string DEF/p def-str))
  
  )
