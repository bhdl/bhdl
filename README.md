# Board HDL: A Programming Language and System for making PCBs

Documents can be found at https://bhdl.org

This project is currently developed on a new IDE called CodePod, which is not publicly available yet. The `src` folder here is exported from CodePod IDE. We're working on making CodePod available for the general public. Issues are being tracked in this repo.

BHDL is a programming language embedded in
[racket](https://racket-lang.org/). In addition to the language, the system
consists of a layout co-design system based on functional picture, REPL-driven
interactive development and visualization, libraries, KiCAD compatible exporter,
and placement engines including an optimization-based anlytical placer (ePlace)
and simulated annealing based detailed placer.

# Setup

## Install Racket

Install [racket](https://racket-lang.org/):

```sh
apt install racket
```

On MacOS and Windows, you can [download the installer from the Racket website](https://download.racket-lang.org/), and setup
the commandline properly.

<!-- IMPORTANT: racket v7.8 seems to be broken! The cc-find will return negative values. That's apparently a bug of either racket v7.8 or the bundled pict library. Use v7.7 instead. Links to the download pages:

- [all versions](https://download.racket-lang.org/all-versions.html)
- [v7.7](https://download.racket-lang.org/racket-v7.7.html) -->

## Install BHDL package

Step 2: Install the BHDL directly as a racket package:

```
raco pkg install --auto https://github.com/bhdl/bhdl.git?path=src
```

You can now run racket and require bhdl. The first time requiring bhdl, it will download footprint library to `~/.config/bhdl/bhdl-footprints` (Linux) or `~/Library/Application Support/bhdl/bhdl-footprints` (MacOS).

```racket
racket
> (require bhdl)
```

Then try some of the [examples](/examples).

## Update BHDL

In the future, you can update the package if there're updates upstream via:

```shell
raco pkg update src
```

> note TODO package name should be `bhdl`

## JupyterLab IDE

In general, you can run racket programs via command line, editor plugins, IDEs. We are not covering them here. Instead, we recommand run the program via jupyter notebook. To set it up, first install jupyter:

```shell
pip install jupyterlab
```

The [iracket](https://github.com/rmculpepper/iracket) kernel requires on libzeromq:

```shell
sudo apt install libzmq5
```

> note for MacOS

> On MacOS, it is a little [trickier](https://github.com/rmculpepper/racket-zeromq/issues/6) to setup libzmq. Specifically, you need to copy the library to the racket's library path, otherwise racket won't find it:

> ```shell
> brew install zmq
> cp /opt/homebrew/Cellar/zeromq/4.3.4/lib/libzmq.5.dylib Library/Racket/8.2/lib
> ```

Then install the kernel itself:

```shell
raco pkg install iracket
raco iracket install
```

> note:
> iracket does not allow write access to file system by default. So you have to
> modify the kernel file with a [`-t`
> parameter](https://github.com/rmculpepper/iracket/issues/13). In short, you need
> to modify the content of `~/.local/share/jupyter/kernels/racket/kernel.json` on
> your system from `["racket","-l","iracket/iracket","--","{connection_file}"]` to
> `["racket","-l","iracket/iracket","--","-t","{connection_file}"]`

Then start the jupyterlab server:

```shell
jupyter lab
```

You'll be able to create racket notebooks in the JupyterLab IDE. Inside the
notebook, try `(require bhdl)` and some of [the exampels](/examples).

<!-- TODO rename fitboard to BHDL-Key -->

<!-- - [BHDL-Key](bhdl-test/fitboard.ipynb): an ergonomic keyboard
- [onebutton](bhdl-test/onebutton.ipynb): a pushbutton board: https://github.com/forrestbao/onebutton
- [Arduino Spreadboard](bhdl-test/spreadboard.ipynb): an multi-dock for different form-factor Arduinos -->

<!-- One caveat to notice: the iRacket kernel seems to have problem when "restarting the kernel". The walk-around is to "shutdown the kernel" and "start it again". -->

## Placement (Optional)

To run the placement engine, you need setup
[julia](https://julialang.org/). Refer to their installation guide on how to
install Julia.

Install the BHDL julia package:

```
julia
]add https://github.com/bhdl/bhdl
```

Then run the server:

```
import BHDL
BHDL.web_server()
```

This will listen on `http://0.0.0.0:8082`. To use this, in racket, set the
parameter `placer-url`, and call `circuit-export` with `#:auto-place #t`:

```racket
;; set placer url
(placer-url "http://0.0.0.0:8082")
(circuit-export three-button #:auto-place #t ...)
```

This package will detect if Nvidia GPU is available, and use it if possible. The
first time invoking the package, julia will download and instantiate the
dependencies and download CUDA (julia will not use your system's CUDA setup).
The first time the placement is running, Julia will pre-compile the code, thus
maybe a little slower. Subsequent placement request will be much faster (which
is why we use a server instead of invoking the command for every placement request).

## Freerouting (Optional)

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
