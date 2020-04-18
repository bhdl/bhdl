using Luxor
import Dates
using ProgressMeter

macro luxoremacs(body, width=600, height=600)
    quote
        path, io = Base.Filesystem.mktemp()
        close(io)

        path * ".png"
        lfname = path * ".png"
        # local lfname = Luxor._add_ext($(esc(fname)), :png)
        Drawing($width, $height, lfname)
        origin()
        background("white")
        sethue("black")
        $(esc(body))
        finish()
        # instead of preview, I'm printing out something
        println("$lfname")
        println("#<Image: $lfname>")
        # preview()
    end
end

function test_Luxor()
    @luxoremacs begin
        text("Hello world ...")
        circle(Point(0, 0), 200, :stroke)
    end
    @luxoremacs begin
        radius=80
        # this change the current pen
        setdash("dot")
        sethue("gray30")
        # create points for side effect
        A, B = [Point(x, 0) for x in [-radius, radius]]
        # draw a line between two points
        line(A, B, :stroke)
        # O is origin
        circle(O, radius, :stroke)
        # labels around points
        label("A", :NW, A)
        label("O", :N,  O)
        label("B", :NE, B)
        # circle.!!
        circle.([A, O, B], 2, :fill)
        circle.([A, B], 2radius, :stroke)
    end
end

function visualize(nodes, nets, pos)
    # 1. construct a dictionary from nodeID->pos
    # 2. construct a dictionary from nodeID->(width,height)
    node_dict = Dict()
    for (name, width, height, _) in nodes
        # FIXME parse in the bench reader
        node_dict[name] = (width, height)
    end
    pos_dict = Dict()
    for (name, x, y, _) in pos
        pos_dict[name] = (x,y)
    end

    xmax = -Inf
    ymax = -Inf
    xmin = Inf
    ymin = Inf
    for node in node_dict
        name = node[1]
        w, h = node[2]
        x, y = pos_dict[name]
        xmin = min(xmin, x-w/2)
        xmax = max(xmax, x+w/2)
        ymin = min(ymin, y-h/2)
        ymax = max(ymax, y+h/2)
    end

    xshift = (xmax - xmin) / 2
    yshift = (ymax - ymin) / 2
    scale = 500 / max(xmax - xmin, ymax - ymin)

    # do the drawing
    @luxoremacs begin
        @showprogress 0.1 "drawing .." for node in node_dict
            name = node[1]
            # scale down
            w, h = node[2] .* scale
            x, y = pos_dict[name]
            x = (x - xmin - xshift) * scale
            y = (y - ymin - yshift) * scale
            # @show w, h, x, y
            box(Point(x,y), w, h, :stroke)
        end
    end
end

function visualize(x, y, w, h, R)
    # shift all
    x = x .- R.xmax / 2
    y = y .- R.ymax / 2

    # apply scale
    scale = 500 / max(R.xmax, R.ymax)
    x = x .* scale
    y = y .* scale
    w = w .* scale
    h = h .* scale

    @luxoremacs begin
        setdash("dash")
        box(Point(0,0), R.xmax * scale, R.ymax * scale, :stroke)
        setdash("solid")
        @showprogress 0.1 "drawing .." for i in 1:length(x)
            # this x y is center
            box(Point(x[i],y[i]), w[i], h[i], :stroke)
        end
    end
end

function test()
    @luxoremacs begin
        box(Point(0,0), 100, 100, :stroke)
        box(Point(50,50), 20, 20, :stroke)
    end
end
