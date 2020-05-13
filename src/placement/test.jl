include("bench.jl")
include("visualize.jl")
include("place.jl")
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



function test()
    str = open("/tmp/rackematic/out/gh60.json") do io
        read(io, String)
    end;
    jobj = JSON.parse(str)

    xs, ys, ws, hs, Es, mask, diearea = decode_place_spec(jobj);

    # place(xs, ys, ws, hs, Es, mask, diearea, vis=true)

    @time solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea, vis=true, iter=200)
    # save to json file
    res_payload = Dict("xs"=>solxs, "ys"=>solys) |> JSON.json
    open("/tmp/rackematic/out/gh60-sol.json", "w") do io
        write(io, res_payload)
    end
end

