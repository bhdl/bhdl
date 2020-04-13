#lang racket

(define (gen-nodes cells Hmacros)
  (string-join (list "UCLA nodes 1.0"
                     (~a "NumNodes : " (length cells))
                     (~a "NumTerminals : 0")
                     (string-join (for/list ([c cells])
                                    (let ([m (hash-ref Hmacros (cell-macro c))])
                                      (~a "  " (cell-name c)
                                          " " (exact->inexact (macro-w m))
                                          " " (exact->inexact (macro-h m)))))
                                  "\n"))
               "\n"))
(define (gen-nets nets)
  (string-join (list "UCLA nets 1.0"
                     (~a "NumNets : " (length nets))
                     (~a "NumPins : 0")
                     (string-join (for/list ([net nets])
                                    (string-join
                                     (list
                                      (~a "NetDegree : " (length (Net-pinrefs net))
                                          " " (Net-name net))
                                      (string-join (for/list ([p (Net-pinrefs net)])
                                                     (~a "  " (Pinref-name p) " I : 0 0"))
                                                   "\n"))
                                     "\n"))
                                  "\n"))
               "\n"))

(define (gen-pl cells)
  (string-join
   (list "UCLA pl 1.0"
         (string-join (for/list ([c cells])
                        (~a (cell-name c) " 0 0 : N"))
                      "\n"))
   "\n"))
(define (gen-aux)
  "RowBasedPlacement :  a.nodes  a.nets  a.wts  a.pl  a.scl")

(module+ test)

(define (gen-def cells nets diew dieh)
  (~a (string-join (list "VERSION   5.8 ;"
                         "NAMESCASESENSITIVE ON ;"
                         "DIVIDERCHAR \"/\" ;"
                         "BUSBITCHARS \"[]\" ;"
                         ;; FIXME design name
                         "DESIGN a ;"
                         "UNITS DISTANCE MICRONS 2000 ;"
                         ;; FIXME diearea scale
                         (~a "DIEAREA ( 0 0 ) ( "
                             diew
                             " "
                             dieh " ) ;")
                         ;; add rows
                         (string-join
                          (for/list ([i (in-range 10)])
                            (~a "ROW ROW_" i " CoreSite 0 "
                                (exact->inexact (* i (/ dieh 12)))
                                " FS DO 10 BY 1 STEP 50 0 ;"))
                          "\n"))
                   "\n")
      (string-join (list
                    (~a "COMPONENTS " (length cells) " ;")
                    (string-join (for/list ([cell cells])
                                   (~a "- " (cell-name cell) " "
                                       (cell-macro cell) " "
                                       " + PLACED ( "
                                       (cell-x cell)
                                       " "
                                       (cell-y cell)
                                       ;; FIXME orient
                                       " ) N ;"))
                                 "\n")
                    "END COMPONENTS"
                    (~a "NETS " (length nets) " ;")
                    (string-join (for/list ([net nets])
                                   (~a "- " (Net-name net)
                                       (string-join
                                        (for/list ([pinref (Net-pinrefs net)])
                                          (~a " ( " (Pinref-name pinref)
                                              " "
                                              (~a "P" (Pinref-index pinref))
                                              " ) ")))
                                       "+ USE SIGNAL ;"))
                                 "\n")
                    "END NETS"
                    "END DESIGN")
                   "\n")
      #:separator "\n"))

(define (gen-lef macros)
  (~a (string-join '("VERSION   5.8 ;"
                     "BUSBITCHARS \"[]\" ;"
                     "DIVIDERCHAR \"/\" ;"

                     "UNITS"
                     "CAPACITANCE PICOFARADS 1 ;"
                     "DATABASE MICRONS 2000 ;"
                     "END UNITS"

                     "MANUFACTURINGGRID 0.0005 ;"
                     "LAYER Metal1
  TYPE ROUTING ;
  DIRECTION HORIZONTAL ;
  PITCH 0.19 0.19 ;
  WIDTH 0.06 ;
  AREA 0.02 ;
  SPACINGTABLE
    PARALLELRUNLENGTH 0 
    WIDTH 0    0.06 
    WIDTH 0.1  0.1 
    WIDTH 0.75 0.25 
    WIDTH 1.5  0.45 ;
  SPACING 0.09 ENDOFLINE 0.09 WITHIN 0.025 ;
END Metal1"
                     "SITE CoreSite
  CLASS CORE ;
  SIZE 0.2 BY 1.71 ;
END CoreSite
")
                   "\n")
      (string-join
       (for/list ([m macros])
         (string-join
          (append (list (~a "MACRO " (macro-name m))
                        "  CLASS CORE ;"
                        "  ORIGIN 0 0 ;"
                        (~a "  SIZE "
                            ;; FIXME precision?
                            (exact->inexact (macro-w m))
                            " BY "
                            (exact->inexact (macro-h m))
                            " ;")
                        "  SYMMETRY X Y ;"
                        "  SITE CoreSite ;")
                  (let ([w (macro-w m)]
                        [h (macro-h m)])
                    (for/list ([p (macro-pins m)])
                      (string-join
                       (list (~a "  PIN " (pin-name p))
                             "    PORT"
                             ;; FIXME layer Metal1
                             "      LAYER Metal1 ;"
                             (~a "      RECT "
                                 ;; FIXME use hardcoded number instead of
                                 ;; (pin-offx p) to debug replace crash
                                 (exact->inexact 0) " "
                                 (exact->inexact 0) " "
                                 ;; FIXME no w and h here, add1 is may
                                 ;; not be proper size
                                 " "
                                 (exact->inexact (min 0.5 w))
                                 " "
                                 (exact->inexact (min 0.5 h))
                                 " ;")
                             "    END"
                             (~a "  END " (pin-name p)))
                       "\n")))
                  (list (~a "END " (macro-name m))))
          "\n"))
       "\n")
      "END LIBRARY"
      #:separator "\n"))



;; FIXME a better name
(define (netlist-export netlist lefname defname)
  "From netlist to actual coordinates."
  (define-values (macros cells nets) (netlist->three netlist))
  (list (append '(Macros) macros)
        (append '(Cells) cells)
        (append '(Nets) nets))
  (define Hmacros (for/hash ([m macros])
                    (values (macro-name m) m)))
  (define (cell-wh c)
    (let ([m (hash-ref Hmacros (cell-macro c))])
      (values (macro-w m)
              (macro-h m))))
  (define diearea (for/fold ([area 0])
                            ([c cells])
                    (let-values ([(w h) (cell-wh c)])
                      (+ area (* w h)))))
  (define diewh (inexact->exact (ceiling (sqrt diearea))))
  ;; generate lef
  (call-with-output-file  lefname
    (λ (out)
      (write-string (gen-lef macros) out))
    #:exists 'replace)
  ;; generate def
  (call-with-output-file  defname
    (λ (out)
      (write-string (gen-def cells nets (* diewh 10000) (* diewh 10000)) out))
    #:exists 'replace)
  ;; nodes file
  (call-with-output-file "out/a.nodes"
    (λ (out)
      (write-string (gen-nodes cells Hmacros) out))
    #:exists 'replace)
  ;; nets file
  (call-with-output-file "out/a.nets"
    (λ (out)
      (write-string (gen-nets nets) out))
    #:exists 'replace)
  ;; pl (pos) file
  (call-with-output-file "out/a.pl"
    (λ (out)
      (write-string (gen-pl cells) out))
    #:exists 'replace))

(module+ test
  ;; (make-directory "out")
  (netlist-export mynetlist "out/a.lef" "out/a.def"))
