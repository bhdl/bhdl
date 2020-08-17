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
        circle(Luxor.Point(0, 0), 200, :stroke)
    end
    @luxoremacs begin
        radius=80
        # this change the current pen
        setdash("dot")
        sethue("gray30")
        # create points for side effect
        A, B = [Luxor.Point(x, 0) for x in [-radius, radius]]
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

    @svg begin
        setdash("dash")
        # the point is the center of box
        box(Luxor.Point(0,0), R.xmax * scale, R.ymax * scale, :stroke)
        setdash("solid")
        @showprogress 0.1 "drawing .." for i in 1:length(x)
            # this x y is center
            box(Luxor.Point(x[i],y[i]), w[i], h[i], :stroke)
        end
    end
end

function test()
    @luxoremacs begin
        box(Luxor.Point(0,0), 100, 100, :stroke)
        box(Luxor.Point(50,50), 20, 20, :stroke)
    end
end
