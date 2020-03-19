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
    xs,ys,ws,hs,Es,solxs,solys,mask,mask_offx,mask_offy =
        read_bench("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/", "adaptec1")
    visualize(xs, ys, ws, hs)
    visualize(solxs, solys, ws, hs)
    newxs, newys = place(xs, ys, ws, hs, Es, mask)
    visualize(newxs, newys, ws, hs)
end
