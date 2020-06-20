include("bench.jl")
include("visualize.jl")
include("place.jl")
import Profile
import ProfileView

function test_profile()
    Profile.clear()
    Profile.init(n = 10^7, delay = 0.01)

    open("/tmp/prof.txt", "w") do s
        # I want to ignore all low-cost operations (e.g. <10)
        Profile.print(IOContext(s, :displaysize => (24, 200)), mincount=10)
    end

    # https://discourse.julialang.org/t/profiling-code-reveals-that-most-time-is-used-calling-3-functions-in-task-jl/30751
    #
    # profiling is showing mainly task.jl staff. This does not work:
    #
    # ENV["JULIA_NUM_THREADS"] = 1

    # other visualization methods
    #
    # Profile.print()
    # import ProfileView
    ProfileView.view()
end




function test()
    str = open("/tmp/rackematic/out/gh60.json") do io
        read(io, String)
    end;
    jobj = JSON.parse(str)

    xs, ys, ws, hs, Es, mask, diearea = decode_place_spec(jobj);

    # place(xs, ys, ws, hs, Es, mask, diearea, vis=true)

    @time solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea, vis=true, iter=20)
    Profile.@profile solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea, iter=20)
    # save to json file
    res_payload = Dict("xs"=>solxs, "ys"=>solys) |> JSON.json
    open("/tmp/rackematic/out/gh60-sol.json", "w") do io
        write(io, res_payload)
    end
end

