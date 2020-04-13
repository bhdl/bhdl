using Cairo

function test_cairo()
    c = CairoRGBSurface(256,256);
    cr = CairoContext(c);

    save(cr);
    set_source_rgb(cr,0.8,0.8,0.8);    # light gray
    rectangle(cr,0.0,0.0,256.0,256.0); # background
    fill(cr);
    restore(cr);

    ## original example, following here
    xc = 128.0;
    yc = 128.0;
    radius = 100.0;
    angle1 = 45.0  * (pi/180.0);  # angles are specified
    angle2 = 180.0 * (pi/180.0);  # in radians

    set_line_width(cr, 10.0);
    arc(cr, xc, yc, radius, angle1, angle2);
    stroke(cr);

    # draw helping lines
    set_source_rgba(cr, 1, 0.2, 0.2, 0.6);
    set_line_width(cr, 6.0);

    arc(cr, xc, yc, 10.0, 0, 2*pi);
    fill(cr);

    arc(cr, xc, yc, radius, angle1, angle1);
    line_to(cr, xc, yc);
    arc(cr, xc, yc, radius, angle2, angle2);
    line_to(cr, xc, yc);
    stroke(cr);

    ## mark picture with current date
    move_to(cr,0.0,12.0);
    set_source_rgb(cr, 0,0,0);
    show_text(cr,Libc.strftime(time()));
    write_to_png(c,"sample_arc.png");
end


function Auv(u, v, M)
    # M: number of bins in each axis
    wu = 2 * pi * u / M
    wv = 2 * pi * v / M
    res = 0
    for x in 1:M
        for y in 1:M
            res += rho(x,y, xs, ys, ws, hs) * cos(wu * x) * cons(wv * y)
        end
    end
    return 1 / M^2 * res
end

function Phi(x,y)
    wu = 2 * pi * u / M
    wv = 2 * pi * v / M
    res = 0
    for u in 1:M
        for v in 1:M
            res += Auv(u,v) / (wu^2 + wv^2) * cos(wu * x) + cos(wv * y)
        end
    end
    return res
end

function Ephi_x(x,y)
    wu = 2 * pi * u / M
    wv = 2 * pi * v / M
    res = 0
    for u in 1:M
        for v in 1:M
            res += Auv(u,v) / (wu^2 + wv^2) * cos(wu * x) + cos(wv * y)
        end
    end
    return res
end

function Ephi_y(x,y)
end

import ForwardDiff

function test_diff()
    f(x::Vector) = sum(sin, x) + prod(tan, x) * sum(sqrt, x)
    x = rand(5)
    g = x -> ForwardDiff.gradient(f, x)
    g(x)
    ForwardDiff.hessian(f, x)
end

function potential(vs)
    phi_v(vs)
    phi, Ephix, Ephiy = phi_b(vs)
    vs = xs, ys, ws, hs
    xs, ys, ws, hs = vs
    v = [item[9] for item in vs]
    rhos = rho_v(v, (xs, ys, ws, hs))

    rhos = rho_all(vs)

    rhos = rho_v([item[211440] for item in vs], (xs, ys, ws, hs))

    length(xs)
    [[item[i] for item in vs] for i in 211447-100:211447]
    # what is this?
    auv = FFTW.dct(rhos)
    M = 10

    sum(Ephix)
    sum(Ephiy)
    sum(phi)
    # all of these are 10x10 Complex matrix, but I need gradient w.r.t. xs
    return phi, Ephix, Ephiy
end


function rho_bv(b, v)
    # calculate M,M density matrix for the perticular cell i
    bx, by, bw, bh = b
    vx, vy, vw, vh = v
    xmin = bx * bw
    xmax = (bx+1) * bw
    ymin = by * bh
    ymax = (by+1) * bh
    x1 = max(vx - vw / 2, xmin)
    y1 = max(vy - vh / 2, ymin)
    x2 = min(vx + vw / 2, xmax)
    y2 = min(vy + vh / 2, ymax)
    max(x2 - x1, 0) * max(y2 - y1, 0) / (bw * bh)
end

function rho_v(v, R)
    # FIXME row and column
    res = [rho_bv((bx, by), v, R) for by in 1:M for bx in 1:M]
    reshape(res, M, M)
end

function phi_v(vs)
    error("Deprecated")
    xs, ys, ws, hs = vs
    res_phi = []
    res_Ephix = []
    res_Ephiy = []
    # for i in 1:length(xs)
    for i in 1:length(100)
        v = [item[i] for item in vs]
        M = 10

        rho = rho_v(v, vs)
        auv = FFTW.dct(rho)

        wuv_f(uv) = 2 * pi * uv / M
        wuv2 = reshape([wuv_f(u)^2 + wuv_f(v)^2 for u in 1:M for v in 1:M], M, M)
        wu = reshape([wuv_f(u) for u in 1:M for v in 1:M], M, M)
        wv = reshape([wuv_f(v) for u in 1:M for v in 1:M], M, M)

        # M,M matrix
        phi = FFTW.idct(auv ./ wuv2)
        Ephix = FFTW.idct(auv .* wv ./ wuv2)
        Ephiy = FFTW.idct(auv .* wu ./ wuv2)
        push!(res_phi, sum(phi))
        push!(res_Ephix, sum(Ephix))
        push!(res_Ephiy, sum(Ephiy))
    end
    res_phi, res_Ephix, res_Ephiy
end


function W_grad_x(nets, x)
    f = e->We_grad_x(e,x)
    sum(f.(nets))
end


function test()
    xs
    ys
    ws
    hs
    Es

    We(Es[1], xs, ys)
    We(Es[end], xs, ys)

    We_grad_x(Es[1], xs)
    W_grad_x(Es, xs)
    W(Es, xs, ys)
    W_grad_x_batched(Es, xs)

    f = e->We_grad_x(e, xs)
    f(Es[8])
    sum(f.(Es[8:10000]))

    W(Es, xs, ys)
    W_grad(Es, xs, ys)


    # FIXME performance
    # memory performance
    # TODO I should probably use sparse matrix
    sum((e->We_grad_x(e,xs)).(Es[1:50000]))
    # this solves memory issue, but is slow
    mapreduce(+, Es[1:100000]) do e
        (e->We_grad_x(e,xs))(e)
    end
    
    size(arr[1])
    size(arr[2])
    sum([arr[1], arr[2]])
    
    wes = (net->We(net,xs,ys)).(Es)
    maximum(wes)
    wes[wes.>1e-9]
    # compute gradient
    g = xs->ForwardDiff.gradient(xs->We(Es[1], xs, ys), xs)
    # FIXME why grads are all 0?
    ForwardDiff.gradient(xs->We(Es[1], xs, ys), ys)
    g = xs->ForwardDiff.gradient(xs->W(Es, xs, ys), xs)
    grads = g(xs)
    grads[grads .> 1e-8]
end


function test()
    visualize(xs, ys, ws, hs)
    visualize(solxs, solys, ws, hs)
end

function test()
    We(nets[1])
    We(nets[2])
    length(nets)
    We.(nets[1:100000])
    We.(nets[1:200000])
    # FIXME NaN?
    maximum(We.(nets))
    argmax(We.(nets))
    nets[7808]
    isnan(We(nets[7808]))
    We(nets[7808])
end



Zygote.@adjoint function bell_pxy(vx, vw, bx, bw)
    bell_pxy(vx, vw, bx, bw), function (Δ)
        a = 4 / (vw + 2 * bw) / (vw + 4 * bw)
        b = 2 / bw / (vw + 4 * bw)
        # center-to-center distance of v and b
        dx = abs(vx - (bx * bw - bw / 2))
        res = 0
        if vx > (bx * bw - bw / 2)
            ddx = 1
        else
            ddx = -1
        end
        res = 0
        if dx >= vw / 2 + 2 * bw
            res = 0
        elseif dx >= vw / 2 + bw
            # b * (dx - vw / 2 - 2 * bw)^2
            res = 2 * b * (dx - vw / 2 - 2 * bw) * ddx
        else
            # 1 - a * dx^2
            res = - 2 * a * dx * ddx
        end
        res * Δ, nothing, nothing, nothing
    end
end


# bell-shaped density penalty
function bell_b(vs, b)
    xs, ys, ws, hs = vs
    bx, by = b
    res = map(1:length(xs)) do i
        px = bell_pxy(xs[i], ws[i], bx, R.bw)
        py = bell_pxy(ys[i], hs[i], by, R.bh)
        px * py
    end
    sum(res)
end

function Δbell_b(vs, b)
    xs, ys, ws, hs = vs
    bx, by = b
    res_x = []
    res_y = []
    res = map(1:length(xs)) do i
        px = bell_pxy(xs[i], ws[i], bx, R.bw)
        py = bell_pxy(ys[i], hs[i], by, R.bh)
        Δpx = Δbell_pxy(xs[i], ws[i], bx, R.bw)
        Δpy = Δbell_pxy(ys[i], hs[i], by, R.bh)
        # px * Δpy, py * Δpx
        push!(res_x, py * Δpx)
        push!(res_y, px * Δpy)
    end
    # should just return it for vx
    # sum(res)
    # res
    res_x, res_y
end

function bell_pvb()
    xs, ys, ws, hs = vs
    vs = xs, ys, ws, hs
    bs = [(bx, by) for by in 1:R.M for bx in 1:R.M]
    # FIXME memory?
    # FIXME parallel
    # M*M,
    res = map(bs) do b
        # N,
        map(1:length(xs)) do i
            px = bell_pxy(xs[i], ws[i], b[1], R.bw)
            py = bell_pxy(ys[i], hs[i], b[2], R.bh)
            px * py
        end
    end
    # N, M*M
    # mat = cat(res..., dims=2)
    mat = mycat(res)
    # N,
    qs = ws .* hs
    # (N,)
    #
    # FIXME why all iterms in sum(res, dims=2) is close to 1, and seems to be
    # the same? does that mean I don't need to normalize it?
    cv = qs ./ sum(res, dims=2)
    # this is (M*M,)
    db = sum(mat .* cv, dims=1)
    @info "calculating gradient .."
    res = map(bs) do b
        # N,
        tmp = map(1:length(xs)) do i
            px = bell_pxy(xs[i], ws[i], b[1], R.bw)
            py = bell_pxy(ys[i], hs[i], b[2], R.bh)
            Δpx = Δbell_pxy(xs[i], ws[i], bx, R.bw)
            Δpy = Δbell_pxy(ys[i], hs[i], by, R.bh)
            px * Δpy, py * Δpx
        end
        [t[1] for t in tmp], [t[2] for t in tmp]
    end
    dx = [r[1] for r in res]
    dy = [r[2] for r in res]
    db, sum(dx .* cv, dims=2), sum(dy .* cv, dims=2)
end

function Δbell_pvb()
    xs, ys, ws, hs = vs
    vs = xs, ys, ws, hs
    bs = [(bx, by) for by in 1:R.M for bx in 1:R.M]
    # FIXME memory?
    # FIXME parallel
    # M*M,
    # N, M*M
    # mat = cat(res..., dims=2)
    mat = mycat(res)
    # N,
    qs = ws .* hs
    # (N,)
    #
    # FIXME why all iterms in sum(res, dims=2) is close to 1, and seems to be
    # the same? does that mean I don't need to normalize it?
    cv = qs ./ sum(res, dims=2)
    # this is (M*M,)
    db = sum(mat .* cv, dims=1)
end


# v, b, R
function bell_pxy(vx, vw, bx, bw)
    # FIXME overlap
    a = 4 / (vw + 2 * bw) / (vw + 4 * bw)
    b = 2 / bw / (vw + 4 * bw)
    # center-to-center distance of v and b
    dx = abs(vx - (bx * bw - bw / 2))
    if dx >= vw / 2 + 2 * bw
        0
    elseif dx >= vw / 2 + bw
        b * (dx - vw / 2 - 2 * bw)^2
    else
        1 - a * dx^2
    end
end

function Δbell_pxy(vx, vw, bx, bw)
    a = 4 / (vw + 2 * bw) / (vw + 4 * bw)
    b = 2 / bw / (vw + 4 * bw)
    # center-to-center distance of v and b
    dx = abs(vx - (bx * bw - bw / 2))
    res = 0
    if vx > (bx * bw - bw / 2)
        ddx = 1
    else
        ddx = -1
    end
    if dx >= vw / 2 + 2 * bw
        0
    elseif dx >= vw / 2 + bw
        # b * (dx - vw / 2 - 2 * bw)^2
        2 * b * (dx - vw / 2 - 2 * bw) * ddx
    else
        # 1 - a * dx^2
        - 2 * a * dx * ddx
    end
end

function mycat(aoa)
    # array of array
    arr = similar(aoa[1], size(aoa[1])..., length(aoa))
    for i in 1:length(aoa)
        arr[:,i] = aoa[i]
    end
    # combine the last two dims FIXME the order unchanged?
    # reshape(arr, size(arr)[1:end-2]..., :)
    arr
end

function bell_pvb(vs, R)
    Mb = 0.3 * (R.bw * R.bh)
    xs, ys, ws, hs = vs
    vs = xs, ys, ws, hs
    bs = [(bx, by) for by in 1:R.M for bx in 1:R.M]
    # FIXME memory?
    # FIXME parallel
    # M*M,
    @info "calculating forward density .."
    res = map(bs) do b
        # N,
        map(1:length(xs)) do i
            px = bell_pxy(xs[i], ws[i], b[1], R.bw)
            py = bell_pxy(ys[i], hs[i], b[2], R.bh)
            px * py
        end
    end
    # N, M*M
    # mat = cat(res..., dims=2)
    mat = mycat(res)
    # N,
    qs = ws .* hs
    # (N,)
    #
    # FIXME why all iterms in sum(res, dims=2) is close to 1, and seems to be
    # the same? does that mean I don't need to normalize it?
    cv = qs ./ sum(mat, dims=2)
    # this is (M*M,)
    db = sum(mat .* cv, dims=1)

    @info "calculating gradient .."
    res = map(bs) do b
        # N,
        tmp = map(1:length(xs)) do i
            px = bell_pxy(xs[i], ws[i], b[1], R.bw)
            py = bell_pxy(ys[i], hs[i], b[2], R.bh)
            Δpx = Δbell_pxy(xs[i], ws[i], b[1], R.bw)
            Δpy = Δbell_pxy(ys[i], hs[i], b[2], R.bh)
            px * Δpy, py * Δpx
        end
        [t[1] for t in tmp], [t[2] for t in tmp]
    end
    dx = [r[1] for r in res]
    dy = [r[2] for r in res]
    resx = dropdims(sum(mycat(dx) .* cv, dims=2), dims=2)
    resy = dropdims(sum(mycat(dy) .* cv, dims=2), dims=2)

    # FIXME sum((db .- Mb).^2)
    sum(db), resx, resy
end

function bell_density(vs, R)
    # xs, ys, ws, hs = vs
    # FIXME hard-coded density ratio
    Mb = 0.3 * (R.bw * R.bh)
    # f = (b)->bell_b(vs,b)
    # res = f.([(bx, by) for by in 1:R.M for bx in 1:R.M])
    # sum(res)
    # sum([bell_b(vs, (bx, by)) for by in 1:R.M for bx in 1:R.M])
    db = bell_pvb(vs, R)
    den = sum(db .- Mb)
end

function Δbell_density(vs, R)
    # xs, ys, ws, hs = vs
    # FIXME hard-coded density ratio
    Mb = 0.3 * (R.bw * R.bh)
    f = (b)->Δbell_b(vs,b)
    res = f.([(bx, by) for by in 1:R.M for bx in 1:R.M])
    # sum(res)
    # sum([bell_b(vs, (bx, by)) for by in 1:R.M for bx in 1:R.M])
    sum([r[1] for r in res]), sum([r[2] for r in res])
end

function test()
    d = bell_density((xs, ys, ws, hs), R)
    dx, dy = Δbell_density((xs, ys, ws, hs), R)

    bell_pxy(xs[8], ws[8], 8, R.bw)
    vs = xs, ys, ws, hs
    vs100 = xs[1:100], ys[1:100], ws[1:100], hs[1:100]
    bell_b(vs, (8, R.bw))

    bell_density(vs, R)
    Δbell_density(vs, R)

    d, dx, dy = bell_pvb(vs, R)

    bell_density(vs100, R)

    Δx, Δy = Δbell_density(vs100, R)
    Δx
    Δy
    Δbell_pxy(xs[1], ws[1], 1, R.bw)
    Zygote.gradient(x->bell_pxy(x, ws[1], 1, R.bw), xs[1])
    # this might just work directly
    Zygote.gradient(xs->bell_density((xs, ys, ws, hs), R), xs)
end


function test()
    γ = 1.0
    @info "computing xP, xN"
    # _e
    xP = map(Es) do e
        maximum([xs[i] for i in e])
    end
    xN = map(Es) do e
        minimum([xs[i] for i in e])
    end
    @info "computing aP,aN .."
    # _i(Pin)
    aP = spzeros(length(Es), length(xs))
    aN = spzeros(length(Es), length(xs))
    # FIXME I probably need to maintain a pin map
    @showprogress 0.1 "working .." for (i,e) in enumerate(Es)
        for j in e
            aP[i,j] += exp((xs[j] - xP[i]) / γ)
            aN[i,j] += exp.((xs[j] - xN[i]) / γ)
            # exp.(([xs[j] for j in e] .- xP[i]) ./ γ)
        end
    end
    @info "calculating rest .."
    # _e
    bP = sum(aP, dims=2)
    bN = sum(aN, dims=2)
    # mat version
    Mxs = reshape(xs, 1, length(xs))
    MbP = reshape(bP, length(bP), 1)
    MbN = reshape(bN, length(bN), 1)
    McP = reshape(cP, length(cP), 1)
    McN = reshape(cN, length(cN), 1)
    # _e
    cP = sum(Mxs .* aP, dims=2)
    cN = sum(Mxs .* aN, dims=2)
    # grad we/xi, (e,i)
    grad = spzeros(length(Es), length(xs))

    (1 .+ sparse(Mxs) ./ γ) .* sparse(MbP)

    tmp1 = ((1 .+ Mxs ./ γ) .* MbP .- 1/γ .* McP)
    tmp2 = tmp1 / (MbP).^2 .* MaP
    tmp3 = ((1 .- Mxs ./ γ) .* MbN .+ 1/γ * McN)
    tmp4 = tmp3 / MbN.^2 .* MaN
    grad[e,:] .= tmp2 .- tmp4
    # @showprogress 0.1 "working .." for (e,E) in enumerate(Es)
    #     tmp1 = ((1 .+ xs ./ γ) .* bP[e] .- 1/γ * cP[e])
    #     tmp2 = tmp1 / (bP[e])^2 .* aP[e,:]
    #     tmp3 = ((1 .- xs ./ γ) .* bN[e] .+ 1/γ * cN[e])
    #     tmp4 = tmp3 / (bN[e])^2 .* aN[e,:]
    #     grad[e,:] .= tmp2 .- tmp4
    # end
    sum(grad, dims=1)
end


function rho_b(b, vs, R)
    # x,y should be the bin coordinate
    # this should return N,N for bins
    bx, by = b
    xs, ys, ws, hs = vs
    xmin = bx * R.bw
    xmax = (bx+1) * R.bw
    ymin = by * R.bh
    ymax = (by+1) * R.bh

    all = map(1:length(xs)) do i
        # FIXME overwrite outer variable
        x = xs[i]
        y = ys[i]
        w = ws[i]
        h = hs[i]
        x1 = max(x - w / 2, xmin)
        y1 = max(y - h / 2, ymin)
        x2 = min(x + w / 2, xmax)
        y2 = min(y + h / 2, ymax)
        max(x2 - x1, 0) * max(y2 - y1, 0)
    end
    return sum(all) / (R.bw * R.bh)
end

function rho_all(vs, R)
    # FIXME row and column
    rhos = [rho_b((bx,by), vs, R) for by in 1:R.M for bx in 1:R.M]
    reshape(rhos, R.M, R.M)
end


# https://github.com/JuliaStats/StatsBase.jl
using StatsBase: countmap
# https://github.com/JuliaCollections/DataStructures.jl
using DataStructures: counter

function rho_v_one(x,y,w,h)
    # assuming:
    # bx1s, bx2s, by1s, by2s
    x1 = max.((x - w) / 2, bx1s)
    y1 = max.((y - h) / 2, by1s)
    x2 = min.((x + w) / 2, bx2s)
    y2 = min.((y + h) / 2, by2s)
    max.(x2 - x1, 0) .* max.(y2 - y1, 0)
end

function rho_fast(vs, R)
    # FIXME this is slow, I should go through vs and calculate
    # 1. for (x,y), efficiently decide which bin it belongs to, and use that directly as
    xs, ys, ws, hs = vs
    # vs = xs, ys, ws, hs
    bx = Int.(floor.((xs .- R.xmin) ./ R.bw)) .+ 1
    by = Int.(floor.((ys .- R.ymin) ./ R.bh)) .+ 1
    # FIXME use overlap area
    
    # unique(Pair.(bx, by))
    # counter(Pair.(bx, by))
    c = countmap(Pair.(bx, by))
    res = zeros(R.M, R.M)
    for key in c
        (x,y),ct = key
        # @show x,y,ct
        res[x,y] = ct
    end
    # visualize
    # display_plot(Plots.heatmap(res'))
    res
end

function test()
    vs = xs, ys, ws, hs
    rho = rho_all(vs, R)
    # this is wrong
    rho = rho_fast(vs, R)
    rho = rho_batch(vs, R)

    rho_v_one(xs[1], ys[1], ws[1], hs[1])
    mapreduce(rho_v_one, +, xs, ys, ws, hs)
    mapreduce(rho_v_one, +, xs[1:100], ys[1:100], ws[1:100], hs[1:100])
end


function cell_overlap(x, y, w, h, R)
    # compute cell overlap with bins
    # return list of Pairs: (i,j) -> area
    bx1f = (x - R.xmin - w / 2) / R.bw
    bx2f = (x - R.xmin + w / 2) / R.bw
    by1f = ((y - R.ymin - h / 2) / R.bh)
    by2f = ((y - R.ymin + h / 2) / R.bh)
    bx1 = min(Int(floor(bx1f)) + 1, 1)
    bx2 = max(Int(floor(bx2f)) + 1, R.M)
    by1 = min(Int(floor(by1f)) + 1, 1)
    by2 = max(Int(floor(by2f)) + 1, R.M)
    res = []
    for bx in bx1:bx2
        for by in by1:by2
            x1 = max.(bx1f, (bx-1)*R.bw + R.xmin)
            y1 = max.(by1f, (by-1)*R.bw + R.ymin)
            x2 = min.(bx2f, (bx)*R.bw + R.xmin)
            y2 = min.(by2f, (by)*R.bw + R.ymin)
            area = max.(x2 - x1, 0) .* max.(y2 - y1, 0)
            push!(res, Pair(Pair(bx,by), area))
        end
    end
    res
end

function rho_fast(vs, R)
    xs, ys, ws, hs = vs
    all_pairs = Array{Any}(nothing, length(xs))
    @info "computing pairs .."
    Threads.@threads for i in 1:length(xs)
        all_pairs[i] = cell_overlap(xs[i], ys[i], ws[i], hs[i], R)
    end
    # TODO apply the pairs
    @info "applying pairs .."
    bx = Int.(floor.((xs .- R.xmin) ./ R.bw)) .+ 1
    by = Int.(floor.((ys .- R.ymin) ./ R.bh)) .+ 1
    all_pairs
    res = zeros(R.M, R.M)
    for pairs in all_pairs
        for pair in pairs
            (i,j), area = pair
            res[i,j] += area
        end
    end
    res
end

function test()
    vs = xs, ys, ws, hs
    rho_fast(vs, R)
    rho_batch(vs, R)
end

function back()
    # map x,y into bin
    bxs = Int.(floor.((xs .- R.xmin) ./ R.bw)) .+ 1
    bys = Int.(floor.((ys .- R.ymin) ./ R.bh)) .+ 1
    # calculate phi
    # potential
    phis = ((bx,by)->phi[bx,by]).(bxs, bys)
    # fields
    Ephixs = ((bx,by)->Ephix[bx,by]).(bxs, bys)
    Ephiys = ((bx,by)->Ephiy[bx,by]).(bxs, bys)
    # for each cell (x,y), calculate the overlap with bins and .* phi

    # qi * phis
    qis = ws .* hs
    return qis .* phis, qis .* Ephixs, qis .* Ephiys
end

function test()
    maximum(FFTW.dct(FFTW.dct(rho, 1), 2) .- FFTW.dct(rho))
    real.(FFTW.fft(FFTW.fft(rho, 1), 2))

    maximum(imag.(FFTW.fft(rho, 1)) .- FFTW.dct(rho, 1))
    maximum(real.(FFTW.fft(rho, 1)) .- FFTW.dct(rho, 1))
    maximum(FFTW.dct(rho, 1))
    maximum(real.(FFTW.fft(real.(FFTW.fft(rho, 1), 2))) .- FFTW.dct(rho))

    maximum(FFTW.dct(rho, 1))
end


function test()
    arr = [1 2;3 4]
    FFTW.dct(arr, 1)
    real.(FFTW.fft(arr, 1))
    FFTW.dct([1 2 3 4])
    FFTW.fft([1 2 3 4])
    real.(FFTW.fft([4 3 5 10 5 3]))
    FFTW.dct([4 3 5 10])
end

function rho_batch(vs, R)
    xs, ys, ws, hs = vs
    # OK, I need to use batch
    pxs = reshape(xs, 1, 1, length(xs))
    pys = reshape(ys, 1, 1, length(ys))
    pws = reshape(ws, 1, 1, length(ws))
    phs = reshape(hs, 1, 1, length(hs))
    pxs, pys, pws, phs = (pxs, pys, pws, phs) .|> gpu
    # FIXME this should be computed once
    bx1s = reshape([R.xmin + R.bw * (i-1) for i in 1:R.M for j in 1:R.M], R.M, R.M)
    bx2s = reshape([R.xmin + R.bw * i for i in 1:R.M for j in 1:R.M], R.M, R.M)
    by1s = reshape([R.ymin + R.bh * (j-1) for i in 1:R.M for j in 1:R.M], R.M, R.M)
    by2s = reshape([R.ymin + R.bh * j for i in 1:R.M for j in 1:R.M], R.M, R.M)
    bx1s, by1s, bx2s, by2s = (bx1s, by1s, bx2s, by2s) .|> gpu
    # FIXME do I need to reshape this as well? It may be reshaped automatically
    # during broadcast
    #
    # pbx1s = reshape(bx1s, R.M, R.M, 1)
    batch_size = 3000
    N = length(xs)
    ct = Int(ceil(N / batch_size))
    # res = zeros(R.M, R.M)
    # @showprogress 0.1 "batching .."
    @info "computing using batch .."
    # TODO how about using threads to do the computation?
    res = @showprogress 0.1 "batching .." map(1:ct) do i
        start = (i-1) * batch_size + 1
        stop = min(i * batch_size, N)
        # @show i start stop
        rho_v_many(pxs[:,:,start:stop],
                   pys[:,:,start:stop],
                   pws[:,:,start:stop],
                   phs[:,:,start:stop],
                   bx1s,
                   by1s,
                   bx2s,
                   by2s)
        # res .+= sum()
    end
    @info "summing up" length(res)
    dropdims(sum(cpu(res)), dims=3) |> cpu
end


function test()
    rrr = rho_cells(vs, R)
    display_plot(Plots.heatmap(reverse(rrr[:,:,1], dims=1)))
    display_plot(Plots.heatmap(reverse(rrr[:,:,2], dims=1)))
    display_plot(Plots.heatmap(reverse(rrr[:,:,3], dims=1)))
end


function test()
    # test FFT
    d, dx, dy = density((xs, ys, ws, hs), R)
    # vs = (xs, ys, ws, hs)
    # visualize(phis)
    # visualize(Ephixs)

    display_plot(Plots.heatmap(randn(10,10)))
    display_plot(Plots.heatmap(phi))
    display_plot(Plots.heatmap(Ephix))
end


function test()
    w = W(Es, xs, ys)
    @info "calculating W gradient .."
    wgradx = W_grad_x(Es, xs)
    wgrady = W_grad_x(Es, ys)

    # FIXME this density impl seems to be wrong
    @info "Calculating fft density .."
    @time density((xs, ys, ws, hs), R)
end

function test()
    rho1 = rho_fast((xs, ys, ws, hs), R)
    rho2 = rho_batch((xs, ys, ws, hs), R)
    rho1 .- rho2
end

