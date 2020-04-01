#lang racket


;; reading lef/def files, and do visualization
(require parser-tools/lex
         syntax/parse/define
         racket/trace
         (prefix-in : parser-tools/lex-sre))

(define-tokens
  t (f-constant i-constant string-literal var))
(define-empty-tokens
  et (eof))

(define-lex-abbrevs
  (Comment (:: "#" (:* (:~ (:or #\return #\linefeed)))))
  ;; digit
  (D (:or (:/ "0" "9")))
  (L (:or (:/ "A" "Z")
          (:/ "a" "z")
          "_"))
  (A (:or L D))
  (ES (:or "\\a" "\\b" "\\f" "\\n" "\\r" "\\t" "\\v"
           "\\'" "\\\"" "\\?" "\\\\"))
  (SChar (:or (:~ #\") ES)))

(define get-lexer
  (lexer [whitespace (get-lexer input-port)]
         [Comment (get-lexer input-port)]
         ;; non-important I guess
         [";" 'SEMICOLON]
         ["VERSION" 'VERSION]
         ["BUSBITCHARS" 'BUSBITCHARS]
         ["DIVIDERCHAR" 'DIVIDERCHAR]
         ["UNITS" 'UNITS]
         ["CAPACITANCE" 'CAPACITANCE]
         ["DATABASE" 'DATABASE]
         ["END" 'END]
         ["PICOFARADS" 'PICOFARADS]
         ["MICRONS" 'MICRONS]
         ["MANUFACTURINGGRID" 'MANUFACTURINGGRID]
         ["LAYER" 'LAYER]
         ["TYPE" 'TYPE]
         ["ROUTING" 'ROUTING]
         ["DIRECTION" 'DIRECTION]
         ["HORIZONTAL" 'HORIZONTAL]
         ["PITCH" 'PITCH]
         ["WIDTH" 'WIDTH]
         ["AREA" 'AREA]
         ["SPACINGTABLE" 'SPACINGTABLE]
         ["PARALLELRUNLENGTH" 'PARALLELRUNLENGTH]
         ["SPACING" 'SPACING]
         ["ENDOFLINE" 'ENDOFLINE]
         ["WITHIN" 'WITHIN]
         ["VERTICAL" 'VERTICAL]
         ["CUT" 'CUT]
         ["OVERLAP" 'OVERLAP]
         ["RECT" 'RECT]
         ["DEFAULT" 'DEFAULT]
         ["VIA" 'VIA]
         ["SITE" 'SITE]
         ["CoreSite" 'CoreSite]
         ["CLASS" 'CLASS]
         ["CORE" 'CORE]
         ["SIZE" 'SIZE]
         ["BY" 'BY]
         ["MACRO" 'MACRO]
         ["ORIGIN" 'ORIGIN]
         ["FOREIGN" 'FOREIGN]
         ["SYMMETRY" 'SYMMETRY]
         ["PIN" 'PIN]
         ["OUTPUT" 'OUTPUT]
         ["USE" 'USE]
         ["SIGNAL" 'SIGNAL]
         ["PORT" 'PORT]
         ["INPUT" 'INPUT]
         ["SHAPE" 'SHAPE]
         ["LIBRARY" 'LIBRARY]
         ["NETEXPR" 'NETEXPR]
         ["ANTENNADIFFAREA" 'ANTENNADIFFAREA]
         ["SE" 'SE]
         ["E" 'E]
         ;; numbers
         [(:: (:? "-") (:+ D)) (token-i-constant (string->number lexeme))]
         [(:: (:? "-") (:+ D) "." (:+ D)) (token-f-constant (string->number lexeme))]
         [(:: #\" (:* SChar) #\") (token-string-literal lexeme)]
         ;; variable name
         [(:+ A) (token-var lexeme)]
         [(eof) 'eof]
         ;; [any-string (error lexeme)]
         ))

(define (filter-f f)
  (λ (x) (filter f x)))

(define (lex-until lex tok)
  (for* ([i (in-naturals)]
         [t (list (lex))]
         #:break (or (equal? t tok) (equal? t 'eof)))
    (void)))
(define (lex-until-seq2 lex tok1 tok2)
  (let ([prev (lex)])
    (for* ([i (in-naturals)]
           [cur (list (lex))]
           #:break (or (and (equal? prev tok1)
                            (equal? cur tok2))
                       (equal? cur 'eof)))
      (set! prev cur))))

(struct macro
  (name x y w h pins)
  #:mutable
  #:prefab)

(struct pin
  (name rects)
  #:prefab)

(define (parse-lef fname)
  (let ([lex (let ([in (open-input-file "tests/lefdef/ispd18_test1.input.lef")])
               (port-count-lines! in)
               (λ ()
                 (get-lexer in)))])
    (parse-lef-lex lex)))

(define (parse-lef-lex lex)
  ;; basically I'm going to peek the current token, and decide which function to
  ;; call
  (filter
   macro?
   (for*/list ([i (in-naturals)]
               [tok (list (lex))]
               #:break (eq? tok 'eof))
     (case (token-name tok)
       [(VERSION) (lex) (lex)]
       [(BUSBITCHARS DIVIDERCHAR MANUFACTURINGGRID RECT)
        (lex-until lex 'SEMICOLON)]
       [(UNITS) (lex-until-seq2 lex 'END 'UNITS)]
       [(LAYER VIA SITE) (let ([which (lex)])
                           (lex-until-seq2 lex 'END which))]
       ;; mostly, I just want to macros
       [(MACRO) (parse-MACRO lex)]
       [(END) (or (equal? (lex) 'LIBRARY)
                  (equal? (lex) 'eof)
                  (error "Invalid 'END")) (void)]
       [else
        (error (~a "Unrecognized token " tok ", skipping to ;"))]))))

(define (parse-MACRO lex)
  (let* ([name (token-value (lex))]
         [res (macro name 0 0 0 0 '())])
    (for* ([i (in-naturals)]
           [tok (list (lex))]
           #:break (and (eq? tok 'END)
                        (let ([endtok (lex)])
                          (or (equal? name (token-value endtok))
                              (error (~a "Does not match: " name " vs. " endtok))))))
      (case (token-name tok)
        ;; FIXME a syntax that directly matches the token sequence
        [(ORIGIN) (set-macro-x! res (token-value (lex)))
                  (set-macro-y! res (token-value (lex)))
                  (expect-consume lex 'SEMICOLON)]
        [(CLASS ORIGIN FOREIGN SYMMETRY SITE) (lex-until lex 'SEMICOLON)]
        [(SIZE) (set-macro-w! res (token-value (lex)))
                (lex)
                (set-macro-h! res (token-value (lex)))
                (lex)]
        [(PIN) (let ([pin (parse-MACRO-PIN lex)])
                 (set-macro-pins! res (append (macro-pins res) (list pin))))]
        [else (error (~a "Unrecognized token " tok))]))
    res))

(define (parse-MACRO-PIN lex)
  (let ([name (token-value (lex))]
        [res (void)])
    (for* ([i (in-naturals)]
           [tok (list (lex))]
           #:break (and (eq? tok 'END)
                        (let ([t (lex)])
                          (or (equal? (token-value t) name)
                              (error (~a "Does not match: " name " " t))))))
      (case (token-name tok)
        [(DIRECTION USE SHAPE NETEXPR ANTENNADIFFAREA) (lex-until lex 'SEMICOLON)]
        [(PORT) (set! res (parse-MACRO-PIN-PORT lex))]
        [else (error (~a "[WARNING] MACRO PIN " tok))]))
    (pin name res)))

(define (parse-MACRO-PIN-PORT lex)
  (expect-consume lex 'LAYER)
  (lex)
  (expect-consume lex 'SEMICOLON)
  (for*/list ([i (in-naturals)]
              [tok (list (lex))]
              #:break (eq? tok 'END))
    (let ([a (lex)]
          [b (lex)]
          [c (lex)]
          [d (lex)])
      (expect-consume lex 'SEMICOLON)
      (list (token-value a)
            (token-value b)
            (token-value c)
            (token-value d)))))

(define (expect-consume lex tok-name)
  (or (eq? (token-name (lex)) tok-name)
      (error "Expected " tok-name " but " token-name)))

(module+ test
  (define macros (parse-lef "tests/lefdef/ispd18_test1.input.lef"))
  (take macros 10))

