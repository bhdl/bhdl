# BHDL: A Programming Language and System for making PCBs

BHDL is a programming language embedded in
[racket](https://racket-lang.org/). In addition to the language, the system
consists of a layout co-design system based on functional picture, REPL-driven
interactive development and visualization, libraries, KiCAD compatible exporter,
and placement engines including an optimization-based anlytical placer (ePlace)
and simulated annealing based detailed placer.

# Install

## The BHDL library

First, install [racket](https://racket-lang.org/). Install the BHDL directly as
a racket package:


```
raco pkg install git://github.com/lihebi/bhdl/?path=bhdl-lib
```

For development purpose, you might want to clone the repo and install the cloned
local package. This is known as "linking":

```
cd bhdl-lib
raco pkg install
```

This will place a link between the installed packages and this local
directory. Everytime this local code is modified, the update is immediately
available for the package `bhdl`.



## KiCAD footprint libraries
You will then download the kicad library.

- https://github.com/KiCad/kicad-footprints
- https://github.com/forrestbao/arduino-kicad-library
- https://github.com/sparkfun/SparkFun-KiCad-Libraries

And you need to set the environement variable `BHDL_LIBRARY_PATH` to
point to the right path:

```
export BHDL_LIBRARY_PATH=/path/to/bhdl/bhdl-footprints
```


## (optional) placement engine setup
To run the placement engine, you need setup julia. In the `placement` folder,
install the package by:


```
julia --project
]instantiate
```

Then run the placement server:

```
julia --project server.jl
```

In racket, the `send-for-placement` API call will send placement request to the
backend placer.

## (optional) freerouting
To use routing, you need to have
[freerouting](https://github.com/freerouting/freerouting) available. We tested
on
[freerouting-v1.4.4](https://github.com/freerouting/freerouting/releases/tag/v1.4.4). Make
sure `freerouting-1.4.4-executable.jar` is available in your `$PATH`.

# Run
The following two boards are available for tests:

- [[file:bhdl-test/fitboard.rkt]]: ergonimic keyboard
- [[file:bhdl-test/spreadboard.rkt]]: spreadboard
- [[file:bhdl-test/test-fp-kicad.rkt]]: this will visualize rendered KiCAD
  footprints

There are three ways to run the code.

First, you can run in command line. Run the top-level script:

```
racket fitboard.rkt
```

And run the `test` submodule:

```
raco test fitboard.rkt
```

Second, you can run in [Dr. Racket](https://racket-lang.org/). Open the file and
click `Run`. The `test` submodule will be run.

Last, you can also run via Emacs. If you wonder how to do that, let me know.

When you run the code, you will see some visualizations on REPL, and freerouting
window will pop up, the KiCAD file will be output to `/tmp/bhdl/out.kicad_pcb`.

The Dr. Racket REPL and Emacs REPL supports images. So you can view the image
via e.g.

```lisp
(Atom-pict (make-IC-atom Arduino-Uno))
;; assuming whole is defined as in the example scripts
(Composite-pict whole)
```

# API document

## importing the library
You include the library via

```lisp
(require bhdl)
```

Since `bhdl` re-exposes `*-append` and `*-superimpose` functions to be generic
to `bhdl` components and circuits, if you want to import `pict`, you need to
prefix it, e.g.:

```lisp
(require (prefix-in pict: pict))
```

The language `bhdl/splicing` is not BHDL syntax. It provides a simple "splicing
syntax" for function application:

```lisp
#lang reader bhdl/splicing

(list 1 2 (list 3 4) .. 5)
;; equivalent to
(list 1 2 3 4 5)
;; which evaluates to => '(1 2 3 4 5)
```

## Basic Concepts

There are two first-class types in the BHDL to represent circuits, Atoms and
Composites. An `Atom` is a single component, while `Composite` represents a
circuit consisting of some `Atoms` and `Composites` and a netlist specifying
connections. An `Atom` has pins, and `Composite` also has external pins that is
visible for connections.

```lisp
(struct Atom
  (pinhash [pict #:auto]))
```

```lisp
(struct Composite
  (pinhash nets [pict #:auto]))
```

## Connection syntax and semantics
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

## Component library

### Discrete components
We provide the following functions to create regular electronic components (in
[bhdl-lib/bhdl/library.rkt](bhdl-lib/bhdl/library.rkt)):

```lisp
(R value) ; resistor
(C value) ; capacitor
(crystal)
(fuse value)
(led color)
(diode)
(switch)
(cherry spacing) ; keyboard switch
(connector num)  ; pin header connector

(usb type)       ; various of usb types including:
;; a-male a-female c-male c-female
;; micro-male micro-female
;; mini-male mini-female
```

### IC components
Besides discrete components, we also provide IC components in
[bhdl-lib/bhdl/library-IC.rkt](bhdl-lib/bhdl/library-IC.rkt) Some IC-like
components like Arduino are also included. The IC instance is created by:

```lisp
(make-IC-atom ATmega128)
(make-IC-atom Arduino-Uno)
```

Here is an example definition of ATtiny25, ATtiny45, and ATtiny85 (all has
exactly the same pin-out).

```lisp
(define/IC (ATtiny25 ATtiny45 ATtiny85)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf"
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

There are often many footprints available for a component. You can assign the
footprint when you create the component (TODO).

## define-Composite wrapper syntax

This syntax makes it easy to define a composite. The syntax is:

```lisp
(define-Composite comp
  #:external-pins (o1 o2)
  #:vars ([a (R 22)]
          [b (C 1)]
          [c (crystal)])
  #:connect (*- self.o1 a b c self.o2)
  #:layout (hc-append a b c))
```

This declares a Composite named `comp`, with external pins named `o1` and `o2`
respectively. It contains a resistor, a capacitor, and a crystal, lined together
linearly. In the meantime, the physical layout of `comp` is defined as
horizontally append the three components.

## Layout co-design

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

```
(rotate item 3.14)
(pin-over base dx dy item)
```

## Auto Placement
We implemented an analytical global placer and a simulated annealing detailed
placer. To use them, follow the above installation guide to setup the julia
environment and run the server in the backend:

```
julia --project server.jl
```

You can then send the placement request to backend server via:
```lisp
(define place-result
    (send-for-placement
     (Composite->place-spec whole
                            #:place-nsteps 50
                            #:place-nbins 300
                            #:sa-ncycles 10
                            #:sa-nsteps 3000
                            #:sa-stepsize 10
                            #:sa-theta-stepsize 0.3)))
```

You can visualize the placed Composite via:

```lisp
(Composite->pict whole place-result)
```

## Visualization and export
Once you have placement result, you can generate `kicad_pcb` file:

```lisp
(Composite->kicad-pcb whole place-result)
```

And generate Spectre format for routing:

```lisp
(Composite->dsn whole place-result)
```

They return strings.  If you want to save it to a file:

```lisp
(call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole place-result)
                    out)))
```

and

```lisp
(call-with-output-file "out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole place-result)
                    out)))
```

Finally, call the freerouter:

```lisp
(system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 5")
```

The nice router window will pop up and does its job.
