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