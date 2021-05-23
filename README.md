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
  - [BHDL-Key](bhdl-test/BHDL-Key.ipynb): an ergonomic keyboard
  - [onebutton](bhdl-test/onebutton.ipynb): a pushbutton board: https://github.com/forrestbao/onebutton
  - [Arduino Spreadboard](bhdl-test/spreadboard.ipynb): an multi-dock for different form-factor Arduinos


# Development

A dev container is described in .devcontainer. I found using VSCode's `reopen in
container` awkward. So the best practice is:

```
cd .devcontainer
docker-compose up -d --build
```

Now, go to http://localhost:8889 to see the jupyterlab interface. This local
`bhdl` folder is mounted under the `$HOME` inside the container. BHDL pkg is NOT
installed by default, thus, inside the container:

```
cd bhdl
raco pkg install --auto
```

This will install the bhdl for development. Changes of the bhdl source code will
immediately available for use via `(require bhdl)`.