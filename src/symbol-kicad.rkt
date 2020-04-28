#lang racket

;; read kicad symbol libraries

(require (for-syntax syntax/parse
                     racket/string)
         megaparsack
         ;; megaparsack/text

         data/monad
         data/applicative

         ;; parsack

         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)

         syntax/parse/define
         rackunit
         (except-in pict blank))

;; configuration
(define kicad-symbol-path
  (make-parameter (expand-user-path "~/git/reading/kicad-symbols/")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Well, I still need a lexer to do preprocessing, for:
;; 1. comments
;; 2. ignoring spaces and condensing newlines
;; However, using lexers and tokens creates other problems:
;; 1. I'll need to define different tokens
;; 2. it is impossible to do parser combinator inside token

;; I actually want a lexer than remove whitespace, but keeps newline
(define-tokens simple [IDENTIFIER NUMBER STRING])
(define-empty-tokens simple* [NEWLINE])

(define-lex-abbrevs
  (Comment (:: "#" (:* (:~ (:or #\return #\linefeed)))))
  ;; digit
  (Digit (:or (:/ #\0 #\9)))
  (Letter (:or (:/ #\A #\Z)
          (:/ #\a #\z)))
  (SChar (:or (:~ #\"))))

(define simple-lexer
  (lexer-src-pos
   [Comment (void)]
   [(:: #\newline) 'newline]
   [(:or blank) (void)]
   ;; FIXME only integer
   [(:: (:? #\-) (:+ Digit)) (string->number lexeme)]
   [(:+ (:or
         ;; pin name
         ;; FIXME identifier can even be 2^2 .. but this is supported
         #\~ #\/ #\+ #\^
         ;; in FPLIST pattern
         #\? #\$ #\* #\_ #\- #\.
         Letter Digit)) lexeme]
   [(:: #\" (:* SChar) #\") (token-STRING lexeme)]
   [(eof) eof]))


(define (lex-simple str)
  (let ([in (open-input-string str)])
    (port-count-lines! in)
    (let loop ([v (simple-lexer in)])
      (let ([t (position-token-token v)])
        (cond [(void? t) (loop (simple-lexer in))]
              [(eof-object? t) '()]
              [else (cons v (loop (simple-lexer in)))])))))

(define (lex-simple-reduced str)
  (let ([prev-is-newline #f])
    (reverse
     (foldl (λ (a result)
              (case (position-token-token a)
                [(newline) (case prev-is-newline
                             [(#t) result]
                             [(#f) (set! prev-is-newline #t)
                                   (cons a result)])]
                [else (set! prev-is-newline #f)
                      (cons a result)]))
            '()
            (lex-simple str)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; connect lexer and parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (any->syntax-box item)
  (match item
    [(position-token tok (position offset-a line col) (position offset-b _ _))
     (syntax-box tok (srcloc "DUMMY"
                             ;; #f #f #f #f
                             line col offset-a (- offset-b offset-a)))]))

(define (any-lst->syntax-boxes lst)
  (map any->syntax-box lst))

(define (my-parse parser lst)
  (parse parser (any-lst->syntax-boxes lst)))

(define (parse-string parser str)
  (my-parse parser (lex-simple-reduced str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General parser helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (maybe/p parser)
  (do [res <- (many/p parser #:min 0 #:max 1)]
      (pure (if (empty? res) (void)
                (first res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define newline/p (satisfy/p (λ (s) (eq? s 'newline))))
(define not-newline/p (satisfy/p (λ (s) (not (eq? s 'newline)))))
(define skip-to-newline/p (do (many/p not-newline/p)
                                 (or/p newline/p
                                       eof/p)))


(define (string/p c) (satisfy/p (λ (s) (equal? s c))))
(define string-any/p (satisfy/p (λ (s) (string? s))))
(define (string-not/p c) (satisfy/p (λ (s) (not (equal? s c)))))
(define number/p (satisfy/p (λ (s) (number? s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parser combinators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ALIAS/p (do (string/p "ALIAS")
                    ;; FIXME maybe many aliases
                    ;; CAUTION this can be a single number
                    [id <- (or/p string-any/p
                                 number/p)]
                  skip-to-newline/p
                  (pure id)))

(module+ test
  (parse-string ALIAS/p "ALIAS PIC10F202-IMC")
  (parse-string ALIAS/p "fdjiALIAS PIC10F202-IMC")
  (parse-string (maybe/p ALIAS/p) "hello")
  (parse-string (maybe/p ALIAS/p) "ALIAS hel"))

(define FPLIST/p (do (string/p "$FPLIST")
                     [patterns <- (many/p (string-not/p "$ENDFPLIST"))]
                   (string/p "$ENDFPLIST")
                   skip-to-newline/p
                   ;; FIXME seems ugly to remove newlines here
                   (pure (filter (λ (a) (not (eq? a 'newline)))
                                 patterns))))

(module+ test
  (parse-string FPLIST/p "$FPLIST
 DFN*8*1EP*2x3mm*P0.5mm*
 DFN*8*1EP*2x3mm*P0.5mm*
$ENDFPLIST
"))

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
                     [x1 <- number/p]
                   [y1 <- number/p]
                   [x2 <- number/p]
                   [y2 <- number/p]
                   skip-to-newline/p
                   (pure (draw-rect x1 y1 x2 y2))))
(define X-line/p (do (string/p "X")
                     [name <- string-any/p]
                   [index <- number/p]
                   [xoff <- number/p]
                   [yoff <- number/p]
                   skip-to-newline/p
                   (pure (draw-pin name index xoff yoff))))
(define A-line/p (do (string/p "A")
                     skip-to-newline/p))
(define C-line/p (do (string/p "C")
                     skip-to-newline/p))
(define P-line/p (do (string/p "P")
                     skip-to-newline/p))
(define T-line/p (do (string/p "T")
                     skip-to-newline/p))
(define B-line/p (do (string/p "B")
                     skip-to-newline/p))

(define DRAW-item/p (or/p S-line/p
                          X-line/p
                          ;; FIXME these are not used
                          A-line/p
                          C-line/p
                          P-line/p
                          T-line/p
                          B-line/p))

(define DRAW/p (do (string/p "DRAW")
                   skip-to-newline/p
                 [items <- (many/p DRAW-item/p)]
                 (string/p "ENDDRAW")
                 skip-to-newline/p
                 (pure items)))


;; this is symbol, e.g. U for IC, R for resistor
(define F0/p (do (string/p "F0")
               ;; [symbol <- quoted-string/p]
               skip-to-newline/p
               ;; (pure symbol)
               ))

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
                     ;; PIC10F200-IMC
                       [name <- (or/p string-any/p
                                    ;; CAUTION the name can be just a number
                                    number/p)]
                     skip-to-newline/p
                     (pure (if (number? name)
                               (number->string name)
                               name))))

(define DEF/p (do [name <- DEF-line/p]
                  F0/p
                F1/p
                F2/p
                F3/p
                ;; alias is not important
                ;; and there may be no alias at all
                [alias <- (maybe/p ALIAS/p)]
                ;; Footprint patterns that are valid. This should be important
                [fplist <- FPLIST/p]
                [draw <- DRAW/p]
                (string/p "ENDDEF")
                (pure (kicad-symbol name fplist
                                    (filter draw-rect? draw)
                                    (filter draw-pin? draw)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (parse-kicad-lib fname)
  "Read a kicad mod file, parse it, and return a symbol object."
  (parse-result!
   (parse-string
    ;; FIXME scope of the monad binding variable?
    (do [res <- (many/p (or/p (do [res <- DEF/p]
                                  skip-to-newline/p
                                (pure res))
                              (do (string/p "EESchema-LIBRARY")
                                  skip-to-newline/p))
                        ;; FIXME separated by newlines
                        ;; #:sep newline/p
                        )]
        eof/p
      (pure res))
    (call-with-input-file fname
      (λ (in) (port->string in))))))


(module+ test
  (parse-kicad-lib "/home/hebi/git/reading/kicad-symbols/4xxx.lib"))



