using PackageCompiler

# FIXME this probably would compile all functions in these packages? If that's
# slow, try identify the key functions to compile
create_sysimage([:CuArrays, :Zygote,
                 :Luxor,
                 :JSON,
                 :ProgressMeter,
                 :ProfileView,
                 :Flux
                 ],
                # sysimage_path="myimage.so",
                replace_default=true)

## run this file to create the image
# julia --project precompile.jl

## to use the image
# julia --sysimage myimage.so --project main.jl
# restore_default_sysimage()
