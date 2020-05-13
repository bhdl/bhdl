#lang racket

;; reading lef/def files, and do visualization
(require (except-in parser-tools/lex blank)
         syntax/parse/define
         racket/trace
         racket/draw
         pict
         (prefix-in : parser-tools/lex-sre))

(define-simple-macro (assert e)
  (or e (error (~a "Assertion error: " (syntax->datum #'e)))))

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
         ["NAMESCASESENSITIVE" 'NAMESCASESENSITIVE]
         ;; ["SE" 'SE]
         ;; ["E" 'E]
         ;; def
         ["ROW" 'ROW]
         ["TRACKS" 'TRACKS]
         ["DESIGN" 'DESIGN]
         ["DIEAREA" 'DIEAREA]
         ["COMPONENTS" 'COMPONENTS]
         ["PINS" 'PINS]
         ["SPECIALNETS" 'SPECIALNETS]
         ["NETS" 'NETS]
         ["(" 'l-paren]
         [")" 'r-paren]
         ;; numbers
         [(:: (:? "-") (:+ D)) (token-i-constant (string->number lexeme))]
         [(:: (:? "-") (:+ D) "." (:+ D)) (token-f-constant (string->number lexeme))]
         [(:: #\" (:* SChar) #\") (token-string-literal lexeme)]
         ;; variable name
         [(:+ A) (token-var lexeme)]
         [(eof) 'eof]
         ["-" '-]
         ["+" '+]
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
  (let ([lex (let ([in (open-input-file fname)])
               (port-count-lines! in)
               (λ ()
                 (get-lexer in)))])
    (parse-lef-lex lex)))

(define (parse-def fname)
  (let ([lex (let ([in (open-input-file fname)])
               (port-count-lines! in)
               (λ ()
                 (get-lexer in)))])
    (parse-def-lex lex)))


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
  (let ([t (lex)])
    (or (eq? (token-name t) tok-name)
        (error "Expected " tok-name " but " t))))

(struct cell
  (name macro x y orient)
  #:prefab)
(struct net
  (name insts)
  #:prefab)
(struct inst
  (name pin))

(define (parse-CELL lex)
  (let ([name (token-value (lex))]
        [macro (token-value (lex))])
    ;; skip until l-paren
    (lex-until lex 'l-paren)
    (let ([x (token-value (lex))]
          [y (token-value (lex))]
          [_ (expect-consume lex 'r-paren)]
          [orient (token-value (lex))])
      (expect-consume lex 'SEMICOLON)
      (cell name macro x y orient))))

(define (parse-NET lex)
  (let ([netname (token-value (lex))])
    (let ([insts (for*/list ([i (in-naturals)]
                             [tok (list (lex))]
                             #:break (or (eq? tok 'SEMICOLON)
                                         (and (eq? tok '+)
                                              (lex-until lex 'SEMICOLON))))
                   (assert (eq? tok 'l-paren))
                   (let ([instname (token-value (lex))]
                         [pinname (token-value (lex))])
                     (expect-consume lex 'r-paren)
                     (inst instname pinname)))])
      (net netname insts))))

(define (parse-def-lex lex)
  ;; I'm looking for COMPONENTS (name, pos, orient)
  ;; and NETS (name, (component, pin))
  (let ([cells (void)]
        [nets (void)]
        [diearea '()])
    (for* ([i (in-naturals)]
           [tok (list (lex))]
           #:break (eq? tok 'eof))
      (case (token-name tok)
        [(VERSION) (lex) (lex)]
        [(BUSBITCHARS DIVIDERCHAR DESIGN UNITS ROW TRACKS NAMESCASESENSITIVE)
         (lex-until lex 'SEMICOLON)]
        [(DIEAREA) (let ([_a (expect-consume lex 'l-paren)]
                         [x1 (token-value (lex))]
                         [y1 (token-value (lex))]
                         [_b (expect-consume lex 'r-paren)]
                         [_c (expect-consume lex 'l-paren)]
                         [x2 (token-value (lex))]
                         [y2 (token-value (lex))]
                         [_d (expect-consume lex 'r-paren)]
                         [_e (expect-consume lex 'SEMICOLON)])
                     (set! diearea (list x1 y1 x2 y2)))]
        [(COMPONENTS)
         (set! cells
               (let ([num (token-value (lex))])
                 (expect-consume lex 'SEMICOLON)
                 (for*/list ([i (in-naturals)]
                             [tok (list (lex))]
                             #:break (and (eq? tok 'END)
                                          (let ([t (lex)])
                                            (or (eq? t 'COMPONENTS)
                                                (error "Do not match: " t)))))
                   (or (eq? tok '-)
                       (error (~a "expected - but " tok)))
                   (parse-CELL lex))))]
        [(PINS) (lex-until-seq2 lex 'END 'PINS)]
        [(SPECIALNETS) (lex-until-seq2 lex 'END 'SPECIALNETS)]
        [(NETS)
         (set! nets
               (let ([num (token-value (lex))])
                 (expect-consume lex 'SEMICOLON)
                 (for*/list ([i (in-naturals)]
                             [tok (list (lex))]
                             #:break (and (eq? tok 'END)
                                          (let ([t (lex)])
                                            (or (eq? t 'NETS)
                                                (error (~a "Do not match: " t))))))
                   (or (eq? tok '-)
                       (error (~a "expected - but " tok)))
                   (parse-NET lex))))]
        [(END) (or (equal? (lex) 'DESIGN)
                   (equal? (lex) 'eof)
                   (error "Invalid 'END")) (void)]
        [else
         (error (~a "Unrecognized token " tok ", skipping to ;"))]))
    (values cells nets diearea)))


(define (visualize diearea macros cells nets)
  ;; 1. create hash map for macros
  ;; 2. create hash map for cells
  (define Hmacros (for/hash ([m macros])
                    (values (macro-name m) m)))
  (hash-keys Hmacros)
  (define Hcells (for/hash ([c cells])
                   (values (cell-name c) c)))
  (hash-keys Hcells)
  ;; 1. loop through all cells, and see if its macro is defined in macros
  (for ([c cells])
    (assert (hash-has-key? Hmacros (cell-macro c))))
  (for ([n nets])
    (for ([inst (net-insts n)])
      (assert (hash-has-key? Hcells (inst-name inst)))))
  ;; 1. loop through all macros, and draw pict for it. UDPATE: no need because
  ;; the macro size is tiny compared to die area. Using a fixed rectangle instead.
  (first macros)
  #;
  (define HmacroP (for/hash ([m macros])
                    (values (macro-name m) (draw-macro m))))
  ;; 2. loop through all cells, and get the total area. UPDATE: use diearea
  ;; 3. create a total area
  (define die
    (match diearea
      [(list x1 y1 x2 y2) (rectangle (/ (- x2 x1) 100) (/ (- y2 y1) 100))]))
  (rectangle 390800 383040)

  ;; 12.8, so there is no large macro
  (apply max (for/list ([m macros])
               (macro-w m)))
  ;;
  ;; 4. put all cells onto the correct locations. Looks like I cannot draw a
  ;; pict back to dc, but I can pin-over
  (first cells)
  (define tmp (for/fold ([die die])
                        ([c cells])
                (let* ([x (/ (cell-x c) 100)]
                       [y (/ (cell-y c) 100)]
                       [m (hash-ref Hmacros (cell-macro c))]
                       ;; 2.6 x 1.71
                       [w (* (macro-w m) 10)]
                       [h (* (macro-h m) 10)])
                  (pin-over die x y (rectangle w h)))))
  ;; the nets are not important here
  tmp)

(define (draw-macro-on-dc m dc)
  (send dc draw-rectangle 10 10 30 10)
  #;
  (send dc set-font (make-font #:size 10 #:family 'roman
                               #:weight 'bold))
  (for ([p (macro-pins m)])
    (for ([r (pin-rects p)])
      (match r
        [(list a b c d) (let ([name (pin-name p)]
                              [x (* a 100)]
                              [y (* b 100)]
                              [w (* (- c a) 100)]
                              [h (* (- d b) 100)])
                          ;; (println (~a x " " y " " w " " h))
                          (send dc draw-rectangle
                                x y w h)
                          (send dc draw-text name x y))]))))

(define (draw-macro m)
  ;; (define target (make-bitmap (inexact->exact (* (macro-w m) 1000))
  ;;                             (inexact->exact (* (macro-h m) 1000))))
  ;; (define dc (new bitmap-dc% [bitmap target]))
  ;; (send target save-file "a.png" 'png)
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (define old-font (send dc get-font))
        (draw-macro-on-dc m dc)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen)
        (send dc set-font old-font))
      ;; FIXME this is 2.6 x 1.71, in m probably, I'm converting it
      (inexact->exact (* (macro-w m) 100))
      (inexact->exact (* (macro-h m) 100))))

(module+ test
  (define macros (parse-lef "tests/lefdef/ispd18_test1.input.lef"))
  (length macros)
  (take macros 10)
  (define-values (cells nets diearea)
    (parse-def "tests/lefdef/ispd18_test1.input.def"))
  (define-values (outcells outnets outdiearea)
    (parse-def "tests/lefdef/out.def"))
  (visualize diearea macros cells nets)
  (visualize outdiearea macros outcells outnets)
  (length cells)
  (length nets)
  (take cells 10)
  (take nets 10))

(module+ test
  (define m (first macros))
  (pict? (scale (rectangle 30 30) 0.3))

  (define target (make-bitmap 3000 3000))
  
  ;; (hash-ref Hmacros "SDFFRX4")
  (define p (draw-macro m)))

;; I'm still going to define a lexer for it

(define get-aca-lexer
  (lexer [whitespace (get-aca-lexer input-port)]
         [Comment (get-aca-lexer input-port)]
         ;; non-important I guess
         [":" 'COLON]
         [";" 'SEMICOLON]
         ["UCLA" 'UCLA]
         ["VERSION" 'VERSION]
         ["NumNodes" 'NumNodes]
         ["NumTerminals" 'NumTerminals]
         ["NumNets" 'NumNets]
         ["NumPins" 'NumPins]
         ["NetDegree" 'NetDegree]
         ["terminal" 'terminal]
         ["/FIXED" 'FIXED]
         ;; numbers
         [(:: (:? "-") (:+ D)) (token-i-constant (string->number lexeme))]
         [(:: (:? "-") (:+ D) "." (:+ D)) (token-f-constant (string->number lexeme))]
         ;; variable name
         [(:+ A) (token-var lexeme)]
         [(eof) 'eof]
         ;; [any-string (error lexeme)]
         ))


(struct aca-cell
  (name w h)
  #:prefab)
(struct aca-pos
  (name x y)
  #:prefab)
(struct aca-net
  (name insts)
  #:prefab)

(define (read-cells p)
  (let ([lex (let ([in (open-input-file p)])
               (port-count-lines! in)
               (λ ()
                 (get-aca-lexer in)))]
        [res (void)])
    (for* ([i (in-naturals)]
           [tok (list (lex))]
           #:break (eq? tok 'eof))
      (case (token-name tok)
        [(UCLA) (lex) (lex)]
        [(NumNodes) (lex) (lex)]
        [(NumTerminals) (lex) (lex)
                        (set! res (filter-not
                                   void?
                                   (for*/list ([i (in-naturals)]
                                               [tok (list (lex))]
                                               #:break (eq? tok 'eof))
                                     (case (token-name tok)
                                       ;; FIXME ignoring terminal for now
                                       [(terminal) (void)]
                                       [(var) (let ([name (token-value tok)]
                                                    [w (token-value (lex))]
                                                    [h (token-value (lex))])
                                                (aca-cell name w h))]))))]))
    res))

(define (read-nets p)
  (let ([lex (let ([in (open-input-file p)])
               (port-count-lines! in)
               (λ ()
                 (get-aca-lexer in)))]
        [res (void)])
    (for* ([i (in-naturals)]
           [tok (list (lex))]
           #:break (eq? tok 'eof))
      (case (token-name tok)
        [(UCLA) (lex) (lex) (void)]
        [(NumNets) (lex) (lex) (void)]
        [(NumPins) (lex) (lex) (void)]
        [(NetDegree) (set! res
                           (for*/list ([i (in-naturals)]
                                       [tok (list (lex))]
                                       #:break (eq? tok 'eof))
                             ;; HACK actually 3 unused tokens, but one is in the
                             ;; loop var
                             (lex) (lex)
                             (for*/list ([i (in-naturals)]
                                         [tok (list (lex))]
                                         #:break (or (eq? tok 'eof)
                                                     (eq? tok 'NetDegree)))
                               (let  ([name (token-value tok)]
                                      [dir (token-value (lex))]
                                      [_ (lex)]
                                      [offx (token-value (lex))]
                                      [offy (token-value (lex))])
                                 (list name dir offx offy)))))]))
    res))

(define (read-pos p)
  (let ([lex (let ([in (open-input-file p)])
               (port-count-lines! in)
               (λ ()
                 (get-aca-lexer in)))])
    (for*/list ([i (in-naturals)]
                [tok (list (lex))]
                #:break (eq? tok 'eof))
      (case (token-name tok)
        [(UCLA) (lex) (lex)]
        ;; FIXME this cannot be ignored
        [('FIXED) (void)]
        [(var) (let ([name (token-value tok)]
                     [x (token-value (lex))]
                     [y (token-value (lex))])
                 (lex) (lex)
                 (aca-pos name x y))]))))

(module+ test
  (define aca-dir "/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1")
  (define aca-name "adaptec1")

  ;; 1. read nodes (cells)
  (define aca-cells
    (read-cells (path-add-extension (build-path aca-dir aca-name) ".nodes")))
  (length aca-cells)
  (take aca-cells 10)
  ;; 2. read nets
  ;; FIXME this is very slow, about 10 seconds
  (define aca-nets
    (read-nets (path-add-extension (build-path aca-dir aca-name) ".nets")))
  (length aca-nets)
  (take aca-nets 10)
  ;; 3. read pos
  (define aca-poses
    (read-pos (path-add-extension (build-path aca-dir aca-name) ".pl")))
  (length aca-poses)
  (take aca-poses 10)
  ;; convert them into lef/def
  ;; 1. each cell has a width and height. Turn this into macros?
  ;; 2. each net has pin offset. Turn this into pins in macro?
  ;; 3. the pos is relatively easy, but I need the FIXED to be handled properly
  ;;
  ;; Thus the main difficulty is how to generate proper amount of macros. I'm
  ;; suspending this, and directly generate lef/def for PCB design.
  )
