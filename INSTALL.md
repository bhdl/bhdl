# BHDL Installation Guide

## Installation

First, install [racket](https://racket-lang.org/). Install the BHDL directly as
a racket package:


```
raco pkg install git://github.com/lihebi/bhdl/?path=bhdl-lib
```

In the future, you can update the package if there're updates upstream via:

```
raco pkg update bhdl
```

You will also need the footprint libraries and set the ENV variable
`BHDL_LIBRARY_PATH` to the local path.

```
git clone --recursive https://github.com/lihebi/bhdl-footprints
export BHDL_LIBRARY_PATH=/path/to/bhdl-footprints
```


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
julia --project server.jl
```

## (optional) freerouting

To use routing, you need to have
[freerouting](https://github.com/freerouting/freerouting) available. We tested
on
[freerouting-v1.4.4](https://github.com/freerouting/freerouting/releases/tag/v1.4.4). Make
sure `freerouting-1.4.4-executable.jar` is available in your `$PATH`.
