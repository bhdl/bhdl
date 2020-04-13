include("bench.jl")
include("visualize.jl")
include("place.jl")

function test()
    nodes = read_nodes("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nodes")
    nets = read_nets("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nets")
    pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.pl")
    ans_pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.ntup.pl")

    visualize(nodes, nets, pos)
    visualize(nodes, nets, ans_pos)

    # TODO
    newpos = place(nodes, nets, pos)
    visualize(nodes, nets, newpos)
end

function test()
    xs,ys,ws,hs,Es,mask =
        read_bench("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/",
                   "adaptec1")
    xs,ys,ws,hs,Es,mask =
        read_bench("/home/hebi/data/VLSI-benchmarks/ispd-2005-ut/ispd2005/bigblue4",
                   "bigblue4")
    xs,ys,ws,hs,Es,mask = read_bench("../out/", "a")
    visualize(xs, ys, ws, hs)
    # visualize(solxs, solys, ws, hs)
    # visualize(dream_solx, dream_soly, ws, hs)
    newxs, newys = place(xs, ys, ws, hs, Es, mask)
    visualize(newxs, newys, ws, hs)
    # reading solutions by DREAMplace
    read_pos("/home/hebi/data/VLSI-benchmarks/DREAMPlace/install/results/adaptec1/adaptec1.gp.pl")
end

function test()
    # read json directly for debugging
    str = open("../out/a.json") do io
        read(io, String)
    end
    jobj = JSON.parse(str)

    xs, ys, ws, hs, Es, mask = parse_jobj(jobj)
    Profile.@profile
    @time solxs, solys = place(xs, ys, ws, hs, Es, mask, vis=false)

    visualize(xs, ys, ws, hs, R)
    visualize(solxs, solys, ws, hs, R)

    # this is json output
    res = encode(jobj, solxs, solys)
    open("../out/a-sol.json", "w") do io
        write(io, res)
    end
end

