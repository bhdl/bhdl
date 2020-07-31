import Luxor: Point

isinside(p, a, b) = (b.x - a.x) * (p.y - a.y) > (b.y - a.y) * (p.x - a.x)
 
function intersection(a, b, s, f)
    dc = [a.x - b.x, a.y - b.y]
    dp = [s.x - f.x, s.y - f.y]
    n1 = a.x * b.y - a.y * b.x
    n2 = s.x * f.y - s.y * f.x
    n3 = 1.0 / (dc[1] * dp[2] - dc[2] * dp[1])
    Point((n1 * dp[1] - n2 * dc[1]) * n3, (n1 * dp[2] - n2 * dc[2]) * n3)
end
 
function clipSH(spoly, cpoly)
    outarr = spoly
    q = cpoly[end]
    for p in cpoly
        # @show p
        inarr = outarr
        outarr = Point[]
        # DEBUG if inarr is empty, I assume the two poly does not overlap
        # if isempty(inarr) return Point[] end
        s = inarr[end]
        for vtx in inarr
            if isinside(vtx, q, p)
                if !isinside(s, q, p)
                    push!(outarr, intersection(q, p, s, vtx))
                end
                push!(outarr, vtx)
            elseif isinside(s, q, p)
                push!(outarr, intersection(q, p, s, vtx))
            end
            s = vtx
        end
        q = p
    end
    outarr
end

function isoverlap_old(spoly, cpoly)
    if isempty(clipSH(spoly, cpoly)) return false
    else return true
    end
end

function test()
    isoverlap([Point(50, 150), Point(200, 50), Point(350, 150), Point(350, 300),
               Point(250, 300), Point(200, 250), Point(150, 350), Point(100, 250),
               Point(100, 200)],
              [Point(100, 100), Point(300, 100), Point(300, 300), Point(100, 300)])
    clipSH([Point(50, 150), Point(200, 50), Point(350, 150), Point(350, 300),
            Point(250, 300), Point(200, 250), Point(150, 350), Point(100, 250),
            Point(100, 200)],
           [Point(100, 100), Point(300, 100), Point(300, 300), Point(100, 300)])
    isoverlap([Point(100, 100), Point(300, 100), Point(300, 300), Point(100, 300)],
              [Point(10, 10), Point(20,10), Point(20,20), Point(10, 20)])

    isoverlap([Point(0,0), Point(0,100), Point(100,100), Point(100,0)],
              [Point(200,0), Point(200,100), Point(300,100), Point(300,0)])
    isoverlap([Point(0,0), Point(0,100), Point(100,100), Point(100,0)],
              [Point(50,-50), Point(50,50), Point(150,50), Point(150,-50)])
    clipSH([Point(0,0), Point(0,100), Point(100,100), Point(100,0)],
           [Point(50,-50), Point(50,50), Point(150,50), Point(150,-50)])
    clipSH([Point(0,0), Point(0,100), Point(100,100), Point(100,0)],
           [Point(-50,30), Point(-50,50), Point(150,50)])

    clipSH([Point(100,150), Point(200,250), Point(300,200)],
           [Point(150,150), Point(150,200), 
            Point(200,200), Point(200,150)])
end


# and there's also PolygonClipping.jl
# https://github.com/JuliaGeometry/PolygonClipping.jl
#
# Clipper.jl https://github.com/JuliaGeometry/Clipper.jl
#
# which is a wrapper for Clipper library
# http://www.angusj.com/delphi/clipper.php
#
# some algorithms
# https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
#
# But this snippet does not work (shown above):
# https://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping#Julia
using Clipper
function test()
    path1 = Vector{IntPoint}()
    push!(path1, IntPoint(0, 0))
    push!(path1, IntPoint(0, 1))
    push!(path1, IntPoint(1, 1))
    push!(path1, IntPoint(1, 0))

    path2 = Vector{IntPoint}()
    push!(path2, IntPoint(1, 0))
    push!(path2, IntPoint(1, 1))
    push!(path2, IntPoint(2, 1))
    push!(path2, IntPoint(2, 0))

    c = Clip()
    add_path!(c, path1, PolyTypeSubject, true)
    add_path!(c, path2, PolyTypeSubject, true)

    result, polys = execute(c, ClipTypeUnion, PolyFillTypeEvenOdd, PolyFillTypeEvenOdd)
    # @test result == true
    # @test polys[1][1] == Clipper.IntPoint(0, 0)
    # @test polys[1][2] == Clipper.IntPoint(2, 0)
    # @test polys[1][3] == Clipper.IntPoint(2, 1)
    # @test polys[1][4] == Clipper.IntPoint(0, 1)
end


function isoverlap_clip(poly0, polys...)
    path1 = Vector{IntPoint}()
    for p in poly0
        push!(path1, p)
    end
    c = Clip()
    add_path!(c, path1, PolyTypeSubject, true)

    for poly in polys
        path2 = Vector{IntPoint}()
        for p in poly
            push!(path2, p)
        end
        add_path!(c, path2, PolyTypeClip, true)
    end

    result, polys = execute(c,
                            # ClipTypeUnion,
                            # ClipTypeDifference,
                            ClipTypeIntersection,
                            PolyFillTypeEvenOdd,
                            PolyFillTypeEvenOdd)
    # @show polys
    # FIXME isempty(polys) does not work
    return length(polys) > 0
end

function tuple2intpoint(t)
    map(t) do (x,y)
        IntPoint(round(x*1e6),round(y*1e6))
    end
end

function isoverlap(poly0, polys...)
    isoverlap_clip(tuple2intpoint(poly0), map(tuple2intpoint, polys)...)
end

function test()
    isoverlap([(0,0), (0, 1), (1, 1), (1, 0)],
              [(1, 0), (1, 1), (2, 1), (2, 0)])
    isoverlap([(0,0), (0, 1), (1, 1), (1, 0)],
              [(1, 0), (1, 1), (2, 1), (2, 0)])

    # true, overlap
    isoverlap([(0,0), (0,100), (100,100), (100,0)],
              [(50,-50), (50,50), (150,50), (150,-50)])
    isoverlap([(0,0), (0,100), (100,100), (100,0)],
              [(-50,30), (-50,50), (150,50)])
    isoverlap([(100,150), (200,250), (300,200)],
              [(150,150), (150,200), 
               (200,200), (200,150)])
    # false, non-overlapping
    isoverlap([(0,0), (0,100), (100,100), (100,0)],
              [(200,0), (200,100), (300,100), (300,0)])
end
