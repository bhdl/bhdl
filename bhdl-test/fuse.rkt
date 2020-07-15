; Example from https://github.com/lihebi/bhdl/blob/master/docs/assets/fuse-sch.png


(define-Composite (comp Rv_value Rh_value C_value L_value)
  #:external-pins (FUSE_PWR Vout GND)
  #:vars ([Rv (R Rv_value)] ; vertical resistor, R1, R2, or R3
          [Rh (R Rh_value)] ; vertical resistor, R4, R5, or R6
          [C (C C_value)]   ; first C is a variable, second C is a part name from library
          [L (L L_value)]   ; first L is a variable, second L is a part name from library
          [U (JW5211)]     ; the chip

  #:connect (
              (*- (U.VIN U.EN self.FUSE_PWR)
                  (U.GND self.GND Rv.2 C.2)
                  (U.FB Rv.1 Rh.1)
                  (C.1 Rh.2 L.2 self.out)
                  (U.SW L.1)
              )
            )
  #:layout ; let's ignore layout for now. 
)


