# Racket Tutorial

This is a small guide to get you started on racket, so that you can comfortably
programming in BHDL. You can always refer to the official racket guide and
reference for full documents.

## Previous Writing
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

