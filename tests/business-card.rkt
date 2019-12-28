#lang racket

;; schematic from https://www.thirtythreeforty.net/posts/2019/12/my-business-card-runs-linux/

;; in standard library
(define/binary-component
  R
  L
  C)

;; global pins
(define VCC 'VCC)
(define GND 'GND)



;; fuse

;; custom component
(define/component JW5211
  VIN EN SW FB GND)

(define (λ-FUSE-PWR r-FB-G r-FB-V (use-cap #f))
  (let ((j (JW5211)))
    (group (connect (j.VIN - j.EN)
                    (when use-cap
                      (j.VIN - ((C 2u2)
                                (C 2u2))))
                    (j.GND - GND)
                    (j.SW - (L 2u2) VCC)
                    (j.FB - (R r-FB-G) GND)
                    (j.FB - (R r-FB-V) VCC)
                    (VCC - (C 22u) GND))
           #:pins j.VIN)))

(define (λ-fuse-circuit)
  (let ((f (fuse)))
    (group (connect (- (λ-FUSE-PWR 11k 49k9)
                       (λ-FUSE-PWR 11k 36k)
                       (λ-FUSE-PWR 10k 8k2)
                       f.1))
           #:pins (rename f.2 FUSE_OUT))))


(define/component F1C100s
  (RESET
   ;; power
   VCC_IO UVCC TV_VCC HPVCC AVCC VDD_CORE VCC_DRAM
   AGND EPAD TVGND SVREF
   ;; PC
   (PC0 SPIO_CLK SDC1_CLK)
   (PC1 SPIO_CS SDC1_CMD)
   (PC2 SPIO_MISO SDC1_D0)
   (PC3 SPIO_MOSI UART0_TX)
   ;; PD
   (PD0 LCD_D2 TWI0_SDA RSB_SDA EINTD0)
   (PD1 LCD_D3 UART1_RTS EINTD1)
   (PD2 LCD_D4 UART1_CTS EINTD2)
   (PD3 LCD_D5 UART1_RX EINTD3)
   (PD4 LCD_D6 UART1_TX EINTD4)
   (PD5 LCD_D7 TWI1_SCK EINTD5)
   (PD6 LCD_D10 TWI1_SDA EINTD6)
   (PD7 LCD_D11 DA_MCLK EINTD7)
   (PD8 LCD_D12 DA_BCLK EINTD8)
   (PD9 LCD_D13 DA_LRCK EINTD9)
   (PD10 LCD_D14 DA_IN EINTD10)
   (PD11 LCD_D15 DA_OUT EINTD11)
   (PD12 LCD_D18 TWIO_SCK RSB_SCK EINTD12)
   (PD13 LCD_D19 UART2_TX EINTD13)
   (PD14 LCD_D20 UART2_RX EINTD14)
   (PD15 LCD_D21 UART2_RTS TWI2_SCK EINTD15)
   ;; ...
   ;; USB
   USB-DP USB-DM
   ;; more POWER
   HOSC0 HOSC1
   ;; PE
   (PE0 CSI_HSYNC LCD_D0 TWI2_SCK UART0_RX EINTE0)
   (PE1 CSI_VSYNC LCD_D1 TWI2_SDA UART0_TX EINTE1)
   ;; ...
   (PE11 CLK_OUT TWIO_SCK IR_RX EINTE11)
   (PE12 DA_MCLK TWIO_SDA PWM0 EINTE12)
   ;; PF
   (PF0 SDC0_D1 DBG_MS IR_RX EINTF0)
   (PF1 SDC0_D0 DBG_D1 EINTF1)
   (PF2 SDC0_CLK UART0_RX EINTF2)
   (PF3 SDC0_CMD DBG_D0 EINTF3)
   (PF4 SDC0_D3 UART0_TX EINTF4)
   (PF5 SDC0_D2 DBG_CK PWM1 EINTF5)
   ;; other
   VRA1 VRA2 FMINR FMINL MICIN LINL HPR HPL HPCOM HPCOMFB
   TPX1 TPX2 TPY1 TPY2))

(define/component crystal-4
  ;; NOTE: starting from 1, to match schematic symbols
  (1 2 3 4))

(define (λ-cpu)
  (let ((ic (F1C100s)))
    ;; power group
    (let ((power-group (group (connect (- (- ic.VCC_IO ic.UVCC ic.TV_VCC ic.HPVCC ic.AVCC)
                                          (< (C 2u2)
                                             (C 100n)
                                             (C 100n)
                                             (C 100n)
                                             (C 100n))
                                          GND)
                                       (- ic.AVCC (C 100n) GND)
                                       (- ic.AGND ic.EPAD ic.TVGND GND)
                                       (- ic.SVREF (R 2k2) GND)
                                       (- ic.VDD_CORE
                                          (< (C 100n)
                                             (C 100n)
                                             (C 100n))
                                          GND)
                                       (- ic.VCC_DRAM
                                          (< (C 1u)
                                             (C 1u))
                                          GND)
                                       (- ic.VCC_DRAM (R 2k2) ic.SVREF)
                                       (- ic.SVREF (R 2k2) GND))))
          (power-group-2 (let ((cyt (crystal-4)))
                           (group (connect (- ic.HOSC0 cyt.3 (C 30p) GND)
                                           (- ic.HOSC1 cyt.1 (C 30p) GND)
                                           (- cyt.2 cyt.4 GND)))))
          (power-group-3 (connect (- ic.FMINR ic.FMINL ic.MICIN ic.LINL GND)
                                  (- HPCOMFB GND)
                                  (- TPX1 TPX2 TPY1 TPY2 GND))))
      ;; mulitple groups
      (group power-group
             power-group-2
             power-group-3
             #:pins (RESET
                     (rename PD7 STATUS)
                     ;; SPI pins
                     (rename SPI0_CLK SPI_CLK)
                     (rename SPI0_CS SPI_CS)
                     (rename SPI0_MISO SPI_MISO)
                     (rename SPI0_MOSI SPI_MOSI)
                     ;; usb pins
                     USB-DP
                     USB-DM
                     ;; UART pins
                     (rename UART0_RX UART_RX)
                     (rename UART0_TX UART_TX)
                     ;; IIC pins
                     (rename TWI0_SCK SCL)
                     (rename TWI0_SDA SDA)
                     ;; JTAG pins
                     (rename PF0 JTAG_TMS)
                     (rename PF1 JTAG_TD1)
                     (rename PF3 JTAG_TD0)
                     (rename PF5 JTAG_TCK)
                     )))))

(define/component AT25SF081
  (SO IO1)
  (SI IO0)
  SCK
  CS
  (WP IO2)
  (HOLD IO3)
  VCC GND)

;; the entire schematic
(define (λ-total)
  (let ((cpu (λ-cpu)))
    (let ((j1-group (let ((j1
                           ;; external connectors
                           ;; TODO mark as external interface?
                           (conn UART_TX SPI_CS SPI_MISO RESET
                                 GND SPI_MOSI SPI_CLK UART_RX)))
                      (connect (-
                                ;; TODO auto-match pins with the same name
                                j1.RESET cpu.RESET
                                j1.UART_TX cpu.UART_TX
                                j1.UART_RX cpu.UART_RX
                                j1.SPI_CS cpu.SPI_CS
                                j1.SPI_MISO cpu.SPI_MISO
                                j1.SPI_CLK cpu.SPI_CLK
                                j1.SPI_MOSI cpu.SPI_MOSI
                                j1.GND GND))))
          (j-usb-group (let ((j-USB (usb))
                             (fuse-circuit (λ-fuse-circuit)))
                         (group (connect 
                                 (- j-USB.VBUS fuse-circuit)
                                 (- j-USB.D+ cpu.USB_DP)
                                 (- j-USB.D- cpu.USB_D-)
                                 (- j-USB.GND GND)))))
          (mcu-group (let ((mcu (AT25SF081)))
                       (group (connect (- mcu.VCC mcu.WP mcu.HOLD 3V3)
                                       (- mcu.SI cpu.SPI_MOSI)
                                       (- mcu.SO cpu.SPI_MISO)
                                       (- mcu.SCK cpu.SPI_CLK)
                                       (- mcu.CS cpu.SPI_CS)
                                       (- mcu.GND GND)))))
          (spi-group (connect (- cpu.SPI_CS (R 3k3) 3V3)))
          ;; reset FIXME should this be better put with CPU?
          (reset-group (let ((nm (NMOS-3)))
                         (group (group (connect (- GND nm.2)
                                                (- nm.1 cpu.STATUS (R 49k9) 3V3)
                                                (- nm.3
                                                   ;; FIXME no value
                                                   (R XXX)
                                                   ;; FIXME led direction
                                                   (flip (led))
                                                   3V3)))
                                (group (connect (- 3V3 (R 49k9) cpu.RESET (switch) GND)))))))
      (group j1-group j-usb-group mcu-group reset-group))))


(module+ test
  (schematic->pict (λ-total)))
