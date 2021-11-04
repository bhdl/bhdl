import Pkg
Pkg.status()

import Pkg
Pkg.add("Luxor")
Pkg.add("Clipper")
Pkg.add("ProgressMeter")
Pkg.add("JSON")
Pkg.add("FFTW")
Pkg.add("HTTP")
Pkg.add("ForwardDiff")

Pkg.add("CUDA")

import CUDA
# This will download CUDA
@info CUDA.version()
@info CUDA.has_cuda()


Pkg.add("Flux")
Pkg.add("Zygote")