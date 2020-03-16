# placement

include("bench.jl")

function place(node, nets, pos)
    # return a new pos
end


function test()
    nodes = read_nodes("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nodes")
    nets = read_nets("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nets")
    pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.pl")
    ans_pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.ntup.pl")
    visualize(nodes, nets, pos)
    visualize(nodes, nets, ans_pos)
    newpos = place(nodes, nets, pos)
    visualize(nodes, nets, newpos)
end
