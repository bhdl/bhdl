import JSON


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
                # m = match(r"(o\d+)\t(\d+)\t(\d+)(\tterminal)?", line)
                m = match(r"(X\d+)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)(\s+terminal)?", line)
                
                if m != nothing
                    id = m.captures[1]
                    # seems unimportant
                    width = parse(Float32, m.captures[2])
                    height = parse(Float32, m.captures[3])
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
                m = match(r".*:\s*(\d+)\s*(\w+)", line)
                name = m.captures[2]
                push!(res, cur)
                cur = []
                push!(cur, name)
            else
                # match
                # FIXME what is B? see n112279
                # m = match(r"(o\d+)\s*([IOB]) : (-?\d+\.\d+)\s+(-?\d+\.\d+)", line)
                m = match(r"(X\d+)\s*([IOB]) : (-?(?:\d+\.)?\d+)\s+(-?(?:\d+\.)?\d+)", line)
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
            # m = match(r"(o\d+)\s+(\d+)\s+(\d+).*: \w+( /FIXED)?", line)
            m = match(r"(X\d+)\s+(\d+)\s+(\d+).*: \w+( /FIXED)?", line)
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

function read_bench(folder, name)
    # http://www.ispd.cc/contests/05/ispd05-contest/announcement-jan-12.pdf
    # find
    @info "reading nodes .."
    nodes = read_nodes(joinpath(folder, "$name.nodes"))
    @info "reading nets .."
    nets = read_nets(joinpath(folder, "$name.nets"))
    @info "reading pos .."
    pos = read_pos(joinpath(folder, "$name.pl"))
    # # FIXME there may not be solution
    # sol_pos = read_pos(joinpath(folder, "$name.ntup.pl"))
    # # change to x,y,w,h
    # dream_pos = read_pos("/home/hebi/data/VLSI-benchmarks/DREAMPlace/install/results/adaptec1/adaptec1.gp.pl")

    # FIXME sort?
    # FIXME assert names are in order
    @info "manipulating .."
    name = [n[1] for n in nodes]
    name1 = [p[1] for p in pos]
    name == name1 || error("name order mismatch: $name vs. $name1")
    # FIXME name starts from "0", but that doesn't really matter
    name_dict = Dict(Pair.(name, 1:length(name)))
    x = [p[2] for p in pos]
    y = [p[3] for p in pos]
    # fixed macros
    mask = [if isnothing(p[4]) 1 else 0 end for p in pos]
    mask_offx = [if isnothing(p[4]) 0 else p[2] end for p in pos]
    mask_offy = [if isnothing(p[4]) 0 else p[3] end for p in pos]
    # solution
    # solx = [p[2] for p in sol_pos]
    # soly = [p[3] for p in sol_pos]
    # dream_solx = [p[2] for p in dream_pos]
    # dream_soly = [p[3] for p in dream_pos]
    
    # w, h
    w = [n[2] for n in nodes]
    h = [n[3] for n in nodes]
    # nets
    E = [[name_dict[n[1]] for n in net[2:end]] for net in nets]
    # return x,y,w,h,E,solx,soly,mask,mask_offx,mask_offy, dream_solx, dream_soly
    return x,y,w,h,E,mask
end

function decode_place_spec(jobj)
    xs = jobj["xs"]
    ys = jobj["ys"]
    as = jobj["as"]
    ws = jobj["ws"]
    hs = jobj["hs"]
    Es = jobj["Es"]
    mask = jobj["mask"]
    diearea = jobj["diearea"]
    params = jobj["place-params"]
    xs, ys, as, ws, hs, Es, mask, diearea, params
end