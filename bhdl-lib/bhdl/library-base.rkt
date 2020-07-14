#lang racket

(require racket/contract
         "sch.rkt")

(provide (struct-out IC)
         (struct-out FpSpec)
         (struct-out Connector)
         
         ;; FIXME not sure if these needs to be provided
         (struct-out Resistor)
         (struct-out Capacitor)
         (struct-out ICAtom)
         (struct-out Diode)
         (struct-out CherrySwitch)
         (struct-out LED)
         (struct-out Fuse)
         (struct-out USB))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC definition, for symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct FpSpec
  (fp pins)
  #:prefab)

;; everything should be centered around the IC data structure
;;
;; 1. alts
;;
;; 2. orients: this determines the pin place of the symbol
;;
;; 3. fps: this determines the exact footprint of different packaging
(struct IC
  ;; this tells nothing about the fields. I really need type
  (datasheet alts fps)
  #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atoms used in schematic. This wrap around IC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct Resistor
  (value)
  #:super struct:Atom)

(struct Capacitor
  (value)
  #:super struct:Atom)

(struct LED
  (color)
  #:super struct:Atom)

(struct Diode ()
  #:super struct:Atom)

(struct Fuse
  (value)
  #:super struct:Atom)

(struct Connector
  (num)
  #:super struct:Atom)

(struct USB (type)
  #:super struct:Atom)

(struct ICAtom
  (ic)
  #:super struct:Atom)

(struct CherrySwitch (spacing)
  #:super struct:Atom)

