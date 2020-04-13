Pkg.add("Cairo")
Pkg.add("Luxor")
Pkg.add("ProgressMeter")
Pkg.add("ForwardDiff")
Pkg.add("FFTW")
Pkg.add("Zygote")
Pkg.add("DataStructures")
Pkg.add("StatsBase")
Pkg.add(Pkg.PackageSpec(name="CuArrays", version="1.7.3"))
Pkg.add("CuArrays")
Pkg.add("Flux")
Pkg.add(["CUDAdrv", "CUDAnative", "CuArrays"])
Pkg.test(["CUDAdrv", "CUDAnative", "CuArrays"])
Pkg.add("HTTP")
Pkg.add("JSON")

Pkg.rm("CuArrays")


import Profile
function test_profile()
    Profile.clear()
    Profile.init(n = 10^7, delay = 0.01)

    open("/tmp/prof.txt", "w") do s
        # I want to ignore all low-cost operations (e.g. <10)
        Profile.print(IOContext(s, :displaysize => (24, 200)), mincount=10)
    end

    # other visualization methods
    #
    # Profile.print()
    # import ProfileView
    # ProfileView.view()
end

import Flux
import CuArrays
Flux.gpu(x) = x
Flux.gpu(x) = fmap(CuArrays.cu, x)
