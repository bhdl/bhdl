function read_nodes(fname)
    res = []
    open(fname, "r") do io
        for line in eachline(io)
            line = strip(line)
            if startswith(line, "#")
                continue
            elseif startswith(line, "NumNodes")
                # TODO record
            elseif startswith(line, "NumTerminals")
                # TODO record
            else
                # match
                m = match(r"(o\d+)\t(\d+)\t(\d+)(\tterminal)?", line)
                if m != nothing
                    id = m.captures[1]
                    # seems unimportant
                    width = parse(Int, m.captures[2])
                    height = parse(Int, m.captures[3])
                    # seems duplicate with .pl
                    terminal = m.captures[4]
                    # @show id, x, y, terminal
                    push!(res, (id, width, height, terminal))
                end
            end
        end
    end
    return res
end

function read_nets(fname)
    res = []
    cur = []
    open(fname, "r") do io
        for line in eachline(io)
            line = strip(line)
            if startswith(line, "#")
                continue
            elseif startswith(line, "NumNodes")
                # TODO record
            elseif startswith(line, "NumTerminals")
                # TODO record
            elseif startswith(line, "NetDegree")
                m = match(r".*:\s*(\d+)\s*(n\d+)", line)
                name = m.captures[2]
                push!(res, cur)
                cur = []
                push!(cur, name)
            else
                # match
                m = match(r"(o\d+)\s*([IO]) : (-?\d+\.\d+)\s+(-?\d+\.\d+)", line)
                if m != nothing
                    id = m.captures[1]
                    io = m.captures[2]
                    xoff = parse(Float64, m.captures[3])
                    yoff = parse(Float64, m.captures[4])
                    push!(cur, (id, io, xoff, yoff))
                end
            end
        end
    end
    push!(res, cur)
    # the first is empty
    return res[2:end]
end

function read_pos(fname)
    res = []
    open(fname, "r") do io
        for line in eachline(io)
            line = strip(line)
            m = match(r"(o\d+)\s+(\d+)\s+(\d+).*: \w+( /FIXED)?", line)
            if m != nothing
                id = m.captures[1]
                x = parse(Int, m.captures[2])
                y = parse(Int, m.captures[3])
                fixed = m.captures[4]
                # if fixed != nothing
                push!(res, (id, x, y, fixed))
                # end
            end
        end
    end
    return res
end

function test()
    m = match(r"(o\d+)\s+(\d+)\s+(\d+).*: \w+( /FIXED)?", "o211446	4797	5451	: N /FIXED")
    m.captures[4] != nothing
    match(r"(o\d+)\t(\d+)\t(\d+)(\tterminal)?", line)

    # http://www.ispd.cc/contests/05/ispd05-contest/announcement-jan-12.pdf
    nodes = read_nodes("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nodes")
    length(nodes)
    nets = read_nets("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.nets")
    length(nets)
    pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.pl")
    ans_pos = read_pos("/home/hebi/data/VLSI-benchmarks/ispd-2005/adaptec1/adaptec1.ntup.pl")
    length(pos)
end
