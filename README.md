# BHDL: A Programming Language and System for making PCBs

BHDL is a programming language embedded in
[racket](https://racket-lang.org/). In addition to the language, the system
consists of a layout co-design system based on functional picture, REPL-driven
interactive development and visualization, libraries, KiCAD compatible exporter,
and placement engines including an optimization-based anlytical placer (ePlace)
and simulated annealing based detailed placer.

The online demo server can be found at https://lihebi.xyz. Currently it supports authorized users signing via GitHub OAuth. Contact us (at hebi@lihebi.com) for user account.

Documents:

- User Documents
    - [Installation guide](INSTALL.md)
    - [Getting Started](docs/getting-started.md): this guide presents all you need to compose your circuit through an example keyboard circuit.
    - [Component Library](docs/library.md): the official BHDL components library, and how to create your own library easily.
    - [API reference](docs/API.md): the function APIs including
        - the primary API, `make-circuit`
        - three connection syntax, `*-` `*<` and `*=`
        - layout API
        - exporting API for KiCAD, PNG, PDF, BOM, etc.
- Developer Documents
    - [Advanced API reference](docs/advanced.md): this documents some advanced and internal APIs of BHDL.
    - [Developer and system admin guide](docs/dev.md)
- Example Notebooks:
  - [BHDL-Key](bhdl-test/fitboard.ipynb): an ergonomic keyboard
  - [Arduino Spreadboard](bhdl-test/spreadboard.ipynb): an multi-dock for different form-factor Arduinos
