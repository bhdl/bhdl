#lang racket

(provide dsn-prefix)

(define (dsn-prefix w h)
  `((parser (parser
             (string_quote #\")
             (space_in_quoted_tokens on)
             (host_cad "KiCad's Pcbnew")
             (host_version "5.1.4+dfsg1-1")))
    ;; CAUTION small value (e.g. 1) doesn't work, leave many unrouted
    (resolution um 10)
    (unit um)
    (structure
     (layer F.Cu
            (type signal)
            (property
             (index 0)))
     (layer B.Cu
            (type signal)
            (property
             (index 1)))
     (boundary
      (rect pcb
            ;; CAUTION negative!
            0 ,(- (* h 1000))
            ,(* w 1000) 0))
     (via "Via[0-1]_1000:400_um")
     (rule
      (width 250)
      (clearance 203.3)
      (clearance 203.3 (type default_smd))
      (clearance 50.8 (type smd_smd))))))