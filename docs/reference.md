# BHDL API Documents

You can use the library via

```lisp
(require bhdl)
```

## Component library

A component can be created by calling the corresponding function. This section
refers you to builtin components, and how you can extend the component library
easily.

### Builtin components

Refer to [library-IC.rkt](bhdl-lib/bhdl/private/library-IC.rkt) for the full
list of components.

Basic components:

```
Resistor R Capacitor C Fuse LED LED0603 Diode 1N4148W FerriteBead Crystal-2
Crystal-4
```


ICs:

```
ATtiny25 ATtiny45 ATtiny85 ATmega128 ATmega16 ATmega48 ATmega88 ATmega168
ATmega328 ATmega8U2 ATmega16U2 ATmega32U2 ATmega16U4 ATmega32U4 ATmega8
```

### Defining your own components

Components can be defined via the `define/IC` syntax:

```racket
(define/IC (LED)
  #:FP (fp-diode plus minus)
  #:PREFIX "LED")
```


Notes about the fields:
- `#:FP` expects a footprint and pin names. These pin names are arbitrary but
  the order is important, it will be mapped to the exact order of the
  corresponding footprint. Refer to footprint document for the definition of the
  footprint pin order.
- `#:PREFIX` defines what is the prefix to be appearing on PCB. E.g. R1, LED4

This defines a function called `LED`, and an LED instance can be created via the
function call `(define myled (LED))`. The pins named `plus` and `minus` can be
accessed to make connections. To enalbe a component to connect in "line
connection syntax", you need to specify the #:LEFT and #:RIGHT pins:


```racket
(define/IC (LED)
  #:FP (fp-diode plus minus)
  #:PREFIX "LED"
  #:LEFT plus
  #:RIGHT minus)
```


The left and right delcaration makes it possible to use components of more than
2 pins in the line connection syntax. For exampole, 4 pin switches (where 2
groups are connected)

```racket
(define/IC (SKRPACE010)
  #:FP ((lcsc->fp "C139797")
        A1 A2
        B1 B2)
  #:PREFIX "KEY"
  #:LEFT A1
  #:RIGHT B1)
```


One can change the default left and right orientation easily by defining a
circuit wrapper, more on this on the `make-circuit` document.

Many two-pin non-polarized components do not have meaningful names. E.g. a
resistor just have pin 1 and pin 2. A pin header just have pin 1 2 3 4 ... For
these components, you can write `#:auto-FP` in place of `#:FP`, and the numbers
(starting from 1) will be added for you based on how many pins the footprint
has. You can use 1 and 2 for `#:LEFT` and `#:RIGHT` declaration. E.g.

```racket
(define/IC (Resistor R)
  #:auto-FP (fp-resistor "0603")
  #:PREFIX "R"
  #:LEFT 1
  #:RIGHT 2)
```


There may be many footprints for an IC. Thus you are allowed to specify multiple
footprints:

```racket
(define/IC (Resistor R)
  #:auto-named-FP ("0603" (fp-resistor "0603"))
  #:auto-named-FP ("0805" (fp-resistor "0805"))
  #:PREFIX "R")
```


You can specify which footprint variant to use via
```racket
(R #:FP "0603")
```
  

If not specified, the first one is used.

For ICs, there are two special notes:
1. ICs typically have well-defined footprints. For that, some syntax sugar names
   can be used, e.g. `#:DIP` `#:QFN`.
2. There are often more than one name for a pin of IC. E.g. For ATTiny-serie
   MCUs, the PB0 also functions as `MOSI`, `SDA`, `PCINT0`, and more. This must
   be specified in `#:ALIAS` to delcare they are aliases for the same pin. All
   the pin names will be available for making connections.

```racket
(define/IC (ATtiny25 ATtiny45 ATtiny85)
  #:ALTS ([VCC]
          [GND]
          [PB0 MOSI DI SDA AIN0 OC0A OC1A AREF PCINT0]
          [PB2 SCK USCK SCL ADC1 T0 INT0 PCINT2]
          [PB3 PCINT3 XTAL1 CLKI OC1B ADC3]
          [PB4 PCINT4 XTAL2 CLKO OC1B ADC2]
          [PB5 PCINT5 RESET ADC0 DW])
  #:DIP (8 PB5 PB3 PB4 GND PB0 PB1 PB2 VCC)
  #:QFN (20 PB5 PB3 DNC DNC PB4
            DNC DNC GND DNC DNC
            PB0 PB1 DNC PB2 VCC
            DNC DNC DNC DNC DNC))
```



### footprints

BHDL can [read KiCAD](bhdl-lib/bhdl/private/fp-kicad.rkt) and [read
EasyEDA](bhdl-lib/bhdl/private/fp-easyeda.rkt) footprints. For many components
listed on LCSC.com have associated footprints. We thus support getting the
footprint directly via the ID. E.g.

```racket
(lcsc->fp "C466653")
```


If the local library `$BHDL_LIBRARY_PATH/easyeda` does not contain the
footprint, it will query EasyEDA website and writes the footprint to
`$BHDL_LIBRARY_PATH/easyeda/xxx.json` for caching. As we run it for more
components, our repo will be tracking many components.

This makes it easy to define a new component, e.g. the [1N4148W
diode](https://lcsc.com/product-detail/Switching-Diode_High-Diode-1N4148W_C466653.html) can be defined simply as:


```racket
(define/IC (1N4148W)
  #:FP (lcsc->fp "C466653") (- +)
  #:LEFT +
  #:RIGHT -
  #:PREFIX "D")
```


The order of footprint pins are defined as the orders of the pin occurance in
the KiCAD or EasyEDA footprints, not the name of footprint pins. Although many
footprints have numerical 1 2 3 ..., there are also text name, and there's no
heuristic to sort them.

The choice of using this order of pins has a caveat: the pin order might not be
the actual numerical order. This can be especially comon in KiCAD footprints
where the numerical numbers may be mixed in arbitrary order.



## grow your circuit with`make-circuit`

This syntax makes it easy to define a composite. The syntax is:

```racket
(make-circuit
  #:external-pins (o1 o2)
  #:vars ([a (R 22)]
          [b (C 1)]
          [c (crystal)])
  #:connect (*- self.o1 a b c self.o2)
  #:layout (hc-append a b c))
```

This declares a circuit, with external pins named `o1` and `o2` respectively. It
contains a resistor, a capacitor, and a crystal, lined together linearly. In the
meantime, the physical layout is defined as horizontally append the three
components.

To make the circuit capable of using line connection syntax, define the `left`
and `right` external pins and connect accordingly. E.g.

```racket
(define (Switch)
  (make-circuit
   #:vars ([it (SKRPACE010)])
   #:external-pins (left right)
   #:layout it
   #:connect (list (*- self.left it.A1)
                   (*- self.right it.B1))))
```


### Connection syntax and semantics
Composing circuit is the process of combining smaller circuits and atoms with
netlist. There are 4 syntax for composing Composites. The return value is a
Composite that contains the used components, and the external pin for the
returned Composite is denoted as `out.X`.

The line connection:

```lisp
(*- a b c)
```

Results in the netlist:

```
out.1 -- a.1
a.2 -- b.1
b.2 -- c.1
c.2 -- out.2
```

The split connection:
```lisp
(*< a b c)
```

results in the netlist:

```
out.1 -- a.1 -- b.1 -- c.1
out.2 -- a.2 -- b.2 -- c.2
```

The vectorized connection:
```lisp
(*= (a [p1 p2 p3])
    ([b.p1 c.p2 d.p3]))
```

results in the netlist:
```
a.p1 -- b.p1
a.p2 -- c.p2
a.p3 -- d.p3
```

Note that the vector supports two slightly different syntax: the component can
be write once. I.e. `(a [1 2 3])` is equivalent to `([a.1 a.2 a.3])`.

And finally the netlist syntax:

```lisp
(*+ ([a.1 b.1 c.1]
     [a.2 b.3]))
```

results in the netlis:

```
a.1 -- b.1 -- c.1
a.2 -- b.3
```


### Layout co-design

The layout is inspired by [racket's functional picture
library](https://docs.racket-lang.org/pict/). The following combinators are
provided:

The `*-append` family of functions append its arguments horizontally or
vertically:

```
vl-append
vc-append
vr-append
ht-append
hc-append
hb-append
htl-append
hbl-append
```

The `*-superimpose` family of functions overlap its arguments.

```
lt-superimpose
lb-superimpose
lc-superimpose
ltl-superimpose
lbl-superimpose
rt-superimpose
rb-superimpose
rc-superimpose
rtl-superimpose
rbl-superimpose
ct-superimpose
cb-superimpose
cc-superimpose
ctl-superimpose
cbl-superimpose
```

You can also rotate or pin-over at a absolute location in terms of (x,y)
coordinates:

```racket
(rotate item 3.14)
(pin-over base dx dy item)
```

## Visualization and exporting
The layout can be visualized via

```racket
(show-layout my-circuit)
```


To export KiCAD files, use `circuit-export`:

```racket
(circuit-export fitboard
                #:auto-place #t
                #:formats '(kicad pdf dsn ses))
```



The arguments:
- `#:auto-place` determins whether to run placement engine. This requires a backend
  placement engine running on specific port (by default 8082)
- `#:formats`: this is a list of export formats
  - kicad: kicad_pcb file
  - pdf
  - dsn: Spectre DSN
  - ses: run freerouting on dsn. This requires dsn is exported, freerouting.jar
    executable can be found, and you are running a GUI session so that the
    freerouting.jar window can pop up (which means it cannot run in jupyter
    notebook)
