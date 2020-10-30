# BHDL Installation Guide

## Installation

Step 1: install [racket](https://racket-lang.org/):

```
apt install racket
```

IMPORTANT: racket v7.8 seems to be broken! The cc-find will return negative values. That's apparently a bug of either racket v7.8 or the bundled pict library. Use v7.7 instead. Links to the download pages:
- [all versions](https://download.racket-lang.org/all-versions.html)
- [v7.7](https://download.racket-lang.org/racket-v7.7.html)

Step 2: Install the BHDL directly as a racket package:


```
raco pkg install git://github.com/lihebi/bhdl/?path=bhdl-lib
```

In the future, you can update the package if there're updates upstream via:

```
raco pkg update bhdl-lib
```

You will also need the footprint libraries and set the ENV variable
`BHDL_LIBRARY_PATH` to the local path.

```
git clone --recursive https://github.com/lihebi/bhdl-footprints
export BHDL_LIBRARY_PATH=/path/to/bhdl-footprints
```


## Running via the Jupyter Kernel
In general, you can run racket programs via command line, editor plugins, IDEs. We are not covering them here. Instead, we recommand run the program via jupyter notebook. To set it up, first install jupyter:

```
pip install jupyterlab notebook
```

Then install the iracket kernel:

```
git clone https://github.com/lihebi/iracket
cd iracket && git checkout dev &&\
   raco pkg install --deps search-auto
raco iracket install
```

Note that we made some fix to the upstream iracket kernel ([rmculpepper/iracket](https://github.com/rmculpepper/iracket)), and you need to clone our repo ([lihebi/iracket](https://github.com/lihebi/iracket)) and switch to `dev` branch.

Then start the jupyter notebook server:

```
cd /path/to/bhdl-test
jupyter notebook
```

You might want to open the example notebooks:

<!-- TODO rename fitboard to BHDL-Key -->
- [BHDL-Key](bhdl-test/fitboard.ipynb): an ergonomic keyboard
- [onebutton](bhdl-test/onebutton.ipynb): a pushbutton board: https://github.com/forrestbao/onebutton
- [Arduino Spreadboard](bhdl-test/spreadboard.ipynb): an multi-dock for different form-factor Arduinos

One caveat to notice: the iRacket kernel seems to have problem when "restarting the kernel". The walk-around is to "shutdown the kernel" and "start it again".


## (optional) placement engine

To run the placement engine, you need setup
[julia](https://julialang.org/). Refer to their installation guide on how to
install Julia.

Julia has its own package management and environment, so you can setup the
packages easily. In the `placement` folder, install the package by:

```
julia --project
]instantiate
```

You run the placement server via:

```
cd placement
julia --project main.jl
```

Note that you only need the placement engine when you use the `circuit-export` API with `#:auto-place #t`:

```racket
(circuit-export three-button #:auto-place #t ...)
```

Some future features to notice:
- We will be adding management of placement engine directly in BHDL code
- currently the placement engine uses GPU. We'll be making it compatible with CPU only setup, but that might be substantially slower than on GPU machines.


## (optional) freerouting

To use routing, you need to have
[freerouting](https://github.com/freerouting/freerouting) available. We tested
on
[freerouting-v1.4.4](https://github.com/freerouting/freerouting/releases/tag/v1.4.4). Make
sure `freerouting-1.4.4-executable.jar` is available in your `$PATH`.

The executable can be downloaded [here](https://bintray.com/miho/Freerouting/download_file?file_path=eu%2Fmihosoft%2Ffreerouting%2Ffreerouting%2F1.4.4%2Ffreerouting-1.4.4-executable.jar) on the new freerouting maintainer website: https://freerouting.mihosoft.eu/

UPDATE: the jar file is often not an executable command. You'll need

```
java -jar freerouting-1.4.4-executable.jar
```

Thus, you need to create a executable called `freerouting`, containing:

```
#!/bin/bash
java -jar /path/to/freerouting-1.4.4-executable.jar
```

freerouting will need X11 window, even if we are running it in command line. Simulate the X11 session via:

```
sudo apt install xvfb
Xvfb :1
```

But it seems to be impossible to view the progress. We'll probably need to use VNC if we really want to know what's going on.