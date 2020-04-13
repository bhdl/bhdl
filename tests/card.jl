# business card example

using stdlib: JW5211, R, C, L, USB_B, Polyfuse
import mcu


# global design rules on :labels
design_rule width(:power) = 0.5mm
design_rule width(:FUSE_PWR) = 1.0mm
design_rule length_diff(:USB_signals) = 0

function led_array()
    leds = [LED() for _ in 1:8]
    @circuit (data GND) begin
        @hook_bus map(x->x.anode, leds[1:8]) self.data[1:8]
        @hook self.GND leds.cathode...
    end
end

function fuse_part(r1v, r2v, c1v; c2=false, c3=false)
    j = JW5211()
    r1 = R(r1v)
    r2 = R(r2v)
    c1 = C(c1v)
    c2 = C(1u)
    c3 = C(10u)
    l = L()
    # circuit has lexical scoping. In principle, all components should be
    # created outside of @circuit
    @circuit (GND, Vout, FUSE_PWR) begin
        @hook :power self.Vout l.2 r2.2 c3.1
        @hook j.SW l.1
        @hook j.FB r1.1 r2.1
        @hook :power self.FUSE_PWR j.EN j.VIN

        if c2 && c3
            @hook :power self.FUSE_PWR c2.1 c3.1
            @hook :power self.GND c2.2 c3.2
        end
    end
end


function main()
    usb_jack = USB_B()
    y1 = fuse_part(11k, 49k9, 22u)
    y2 = fuse_part(11k, 36k, 22u)
    y2 = fuse_part 11k 36k 22u
    y3 = fuse_part(10k, 8k2, 22u, 2u2, 2u2)
    f = Polyfuse()
    uno = mcu.arduino_uno()
    leds = led_array()

    @circuit () begin
        @hook :power :USB_VBUS usb_jack.VBUS f.1
        @hook :power :FUSE_PWR y1.FUSE_PWR y2.FUSE_PWR y3.FUSE_PWR f.2
        @hook :GND usb_jack.GND y1.GND y2.GND y3.GND uno.LINL uno.HPCOMFB uno.TPY2 leds.GND
        @hook :FUSE_PWR z.FUSE_PWR y1.FUSE_PWR y2.FUSE_PWR y3.FUSE_PWR

        @hook uno.PD[0:5] leds.data[0:5]
        @hook uno.PD[6:7] leds.data[6:7]
        @hook :USB_signals uno.USB-DP usb_jack.D+
        @hook :USB_signals uno.USB-DN usb_jack.D-
    end
end

# ergonomic keyboard
function key_matrix()
    # qwert yuiop
    # asdfg hjkl
    # zxcvb nm
    # compute locations
    (rotate 30 (v_append 3mm
                (h_append q w e r t)
                (shift-x 5mm
                 (h_append a s d f g))
                (shift-x 10mm
                 (h_append z x c v b))))

    
    
    
end


function test()
    part = main()
    pcb = generate(pcb)
end
