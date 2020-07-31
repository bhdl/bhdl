import FFTW
using SparseArrays: sparse, spzeros
import ForwardDiff
import Zygote
using Statistics: mean
import Plots
import CuArrays: cu, allowscalar
allowscalar(false)
using Flux: gpu, cpu
using GeometricalPredicates

GP = GeometricalPredicates

γ = 0.5

function sumexp(x, γ)
    aP = exp.((x .- maximum(x)) / γ)
    aN = exp.(- (x .- minimum(x)) / γ)
    sum(x .* aP) / sum(aP) - sum(x .* aN) / sum(aN)
end

function We(net, xs, ys)
    # DEBUG using offx and offy
    x = [xs[n] - offx for (n, offx, offy) in net]
    y = [ys[n] - offy for (n, offx, offy) in net]
    wex = sumexp(x, γ)
    wey = sumexp(y, γ)
    return wex + wey
end

function W(nets, xs, ys)
    f = (e)->We(e, xs, ys)
    sum(f.(nets))
end

function We_grad_x_impl(x)
    aP = exp.((x .- maximum(x)) / γ)
    aN = exp.(- (x .- minimum(x)) / γ)
    bP = sum(aP)
    bN = sum(aN)
    cP = sum(x .* aP)
    cN = sum(x .* aN)
    res = (1 .+ x / γ) * bP .- 1/γ * cP
    res2 = (1 .- x / γ) * bN .+ 1 / γ * cN
    return res / (bP ^ 2) .* aP .- res2 / (bN .^ 2) .* aN
end

function We_grad_x(net, x, xory)
    # FIXME why sparse?
    # res = spzeros(length(x))
    res = zeros(length(x))
    # all the x values of net

    if xory == :x
        netx = [x[n] - offx for (n, offx, offy) in net]
    else
        netx = [x[n] - offy for (n, offx, offy) in net]
    end

    grad = We_grad_x_impl(netx)
    # map back to whole x
    for (i,(n,offx,offy)) in enumerate(net)
        res[n] = grad[i]
    end
    res
end

function W_grad_x(nets, x, xory)
    # batched by default
    # f = e->We_grad_x(e,x,xory)
    # res = spzeros(length(x))
    tmp = Array{Any}(nothing, length(nets))

    # export JULIA_NUM_THREADS=4
    #
    # Threads.@threads
    for i in 1:length(nets)
        # tmp[i] = f(nets[i])
        tmp[i] = We_grad_x(nets[i], x, xory)
    end
    sum(tmp)
end

function test()
    Threads.nthreads()
    @time W_grad_x(Es, xs, :x)
end

function rho_v_many(pxs, pys, pws, phs, bx1s, by1s, bx2s, by2s)
    x1 = max.(pxs .- pws / 2, bx1s)
    y1 = max.(pys .- phs / 2, by1s)
    x2 = min.(pxs .+ pws / 2, bx2s)
    y2 = min.(pys .+ phs / 2, by2s)
    # max.(x2 - x1, 0) .* max.(y2 - y1, 0)
    sum(max.(x2 - x1, 0) .* max.(y2 - y1, 0), dims=3)
end

function rho_cells(vs, R)
    xs, ys, ws, hs = vs

    # bx1s = reshape([R.xmin + R.bw * (i-1) for i in 1:R.M for j in 1:R.M], R.M, R.M)
    # bx2s = reshape([R.xmin + R.bw * i for i in 1:R.M for j in 1:R.M], R.M, R.M)
    # by1s = reshape([R.ymin + R.bh * (j-1) for i in 1:R.M for j in 1:R.M], R.M, R.M)
    # by2s = reshape([R.ymin + R.bh * j for i in 1:R.M for j in 1:R.M], R.M, R.M)
    
    pxs = reshape(xs, 1, 1, length(xs))
    pys = reshape(ys, 1, 1, length(ys))
    pws = reshape(ws, 1, 1, length(ws))
    phs = reshape(hs, 1, 1, length(hs))

    # FIXME moving data between cpu and gpu
    x1 = max.(pxs .- pws / 2, R.bx1s)
    y1 = max.(pys .- phs / 2, R.by1s)
    x2 = min.(pxs .+ pws / 2, R.bx2s)
    y2 = min.(pys .+ phs / 2, R.by2s)

    max.(x2 - x1, 0) .* max.(y2 - y1, 0)
end

function rho_fast(vs, R)
    xs, ys, ws, hs = vs
    # OK, I need to use batch
    pxs = reshape(xs, 1, 1, length(xs))
    pys = reshape(ys, 1, 1, length(ys))
    pws = reshape(ws, 1, 1, length(ws))
    phs = reshape(hs, 1, 1, length(hs))
    # FIXME whether to use GPU or not for small scale
    res = rho_v_many(pxs, pys, pws, phs,
                     R.bx1s, R.by1s, R.bx2s, R.by2s)
    dropdims(res, dims=3) |> cpu
end

function phi_b(rho, R)
    # auv = FFTW.dct(rho)
    auv = real.(FFTW.fft(rho))

    # R.M,R.M matrix
    phi = FFTW.idct(auv ./ R.wuv2)

    Ephix = imag.(FFTW.ifft(real.(FFTW.ifft(auv .* R.wv ./ R.wuv2, 1)), 2))
    Ephiy = real.(FFTW.ifft(imag.(FFTW.ifft(auv .* R.wu ./ R.wuv2, 1)), 2))

    phi, Ephix, Ephiy
end

function density(vs, R)
    # vs = xs, ys, ws, hs
    xs, ys, ws, hs = gpu.(vs)
    # 1. calculate rho
    @info "calculating rho .."
    # rho = rho_all(vs, R)
    # rho = rho_fast(vs, R)
    @time rho = rho_fast((xs, ys, ws, hs), R);
    # 2. calculate potential and field using FFT
    @info "calculating potential and field .."
    @time phi, Ephix, Ephiy = phi_b(rho, R);
    # calculate for each v

    # M,M,N
    @time rrr = rho_cells((xs, ys, ws, hs), R);
    d = dropdims(sum(rrr .* gpu(phi), dims=(1,2)), dims=(1,2))
    dx = dropdims(sum(rrr .* gpu(Ephix), dims=(1,2)), dims=(1,2))
    dy = dropdims(sum(rrr .* gpu(Ephiy), dims=(1,2)), dims=(1,2))
    return d, dx, dy
end

struct Region
    # number of bins
    M
    # bin width and height
    bw
    bh
    # overall diearea
    xmin
    xmax
    ymin
    ymax
    # bounding box for bins (to compute only once)
    # TODO when printing out, omit these arrays
    bx1s
    by1s
    bx2s
    by2s
    # for FFT
    wu
    wv
    wuv2
end

function Region(xs, ys, ws, hs, diearea, M)
    xmin = 0
    ymin = 0
    xmax = diearea[1]
    ymax = diearea[2]

    # M = 10
    bw = (xmax-xmin) / M + 1e-8
    bh = (ymax-ymin) / M + 1e-8

    bx1s = reshape([xmin + bw * (i-1) for i in 1:M for j in 1:M], M, M)
    bx2s = reshape([xmin + bw * i for i in 1:M for j in 1:M], M, M)
    by1s = reshape([ymin + bh * (j-1) for i in 1:M for j in 1:M], M, M)
    by2s = reshape([ymin + bh * j for i in 1:M for j in 1:M], M, M)
    bx1s, by1s, bx2s, by2s = (bx1s, by1s, bx2s, by2s) .|> gpu

    # this is slow, but only compute once
    wuv_f(uv) = 2 * pi * uv / M
    wu = reshape([wuv_f(u) for u in 1:M for v in 1:M], M, M);
    wv = reshape([wuv_f(v) for u in 1:M for v in 1:M], M, M);
    wuv2 = reshape([wuv_f(u)^2 + wuv_f(v)^2 for u in 1:M for v in 1:M], M, M);

    Region(M, bw, bh, xmin, xmax, ymin, ymax,
           bx1s, by1s, bx2s, by2s,
           wu, wv, wuv2)
end

function hpwl(xs, ys, Es)
    res = map(Es) do e
        ns = [ei for (ei,offx,offy) in e]
        # max delta x
        abs(maximum((i->xs[i]).(ns)) - minimum((i->xs[i]).(ns))) +
            abs(maximum((i->ys[i]).(ns)) - minimum((i->ys[i]).(ns)))
    end
    sum(res)
end

function display_plot(p)
    path = tempname() * ".png"
    Plots.savefig(p, path)
    println("$(path)")
    println("#<Image: $(path)>")
end

function validate_region!(xs, ys, ws, hs, R)
    # FIXME consider rotation angles
    #
    # FIXME I belive this is pass-by-reference, so that I can modify the values
    # But strange enough, this doesn't work. I have to copy these inline in the
    # two places for now.
    #
    # UPDATE But the fixed locations should not change
    let idx = (xs .- ws ./ 2 .< R.xmin) .& (mask .== 1)
        xs[idx] .= (ws ./ 2 .+ R.xmin)[idx]
    end
    let idx = (xs .+ ws ./ 2 .> R.xmax) .& (mask .== 1)
        xs[idx] .= (ws ./ 2 .- R.xmax)[idx]
    end
    let idx = (ys .- hs ./ 2 .< R.ymin) .& (mask .== 1)
        ys[idx] .= (hs ./ 2 .+ R.ymin)[idx]
    end
    let idx = (ys .+ hs ./ 2 .> R.ymax) .& (mask .== 1)
        ys[idx] .= (hs ./ 2 .- R.ymax)[idx]
    end
end

# return a new pos
function place(xs, ys, ws, hs, Es, mask, diearea; vis=false, nsteps=50, nbins=300)
    xs = Float32.(xs)
    ys = Float32.(ys)
    ws = Float32.(ws)
    hs = Float32.(hs)

    # first, devide into bins
    # FIXME use a more formal way of deciding the bouding box
    # FIXME more bins
    R = Region(xs, ys, ws, hs, diearea, nbins)

    # loss: HPWL and density penalty
    hpwl(xs, ys, Es)
    mask

    # move all to the middle
    midx = (R.xmin + R.xmax) / 2
    midy = (R.ymin + R.ymax) / 2
    xs[mask.==1] .= midx
    ys[mask.==1] .= midy

    # iteratively solve the loss
    # 800 * 10 / 3600 = 2.2 hour
    # FIXME stop criteria: when the update is small enough for several epochs
    for step in 1:nsteps
        @info "step: $step"
        @info "calculating W .."
        w = W(Es, xs, ys)
        @info "calculating W gradient .."
        wgradx = W_grad_x(Es, xs, :x)
        wgrady = W_grad_x(Es, ys, :y)
        wgradx[mask .== 0] .= 0
        wgrady[mask .== 0] .= 0
        # remove the grad for fixed values

        # FIXME this density impl seems to be wrong
        @info "Calculating fft density .."
        d, dx, dy = density((xs, ys, ws, hs), R) .|> cpu
        dx[mask .== 0] .= 0
        dy[mask .== 0] .= 0
        # weights HP here
        dx .*= 0.001
        dy .*= 0.001
        wgradx .*= 10
        wgrady .*= 10

        deltax = wgradx .- dx
        deltay = wgrady .- dy
        # do a cap
        # deltax[deltax .> 10] .= 10
        # deltax[deltax .< -10] .= -10
        # deltay[deltay .> 10] .= 10
        # deltay[deltay .< -10] .= -10
        loss = w + sum(d)

        # DEBUG use only wgrad
        # deltax = wgradx
        # deltay = wgrady
        # loss = w

        @info("data", step, loss,
              mean(abs.(d)), mean(abs.(wgradx)),
              mean(abs.(dx)), hpwl(xs, ys, Es))

        # apply mask for fixed macros
        xs .-= deltax .* mask
        ys .-= deltay .* mask

        # map back to valid region
        # validate_region!(xs, ys, ws, hs, R)
        let idx = (xs .- ws ./ 2 .< R.xmin) .& (mask .== 1)
            xs[idx] .= (ws ./ 2 .+ R.xmin)[idx]
        end
        let idx = (xs .+ ws ./ 2 .> R.xmax) .& (mask .== 1)
            xs[idx] .= (ws ./ 2 .- R.xmax)[idx]
        end
        let idx = (ys .- hs ./ 2 .< R.ymin) .& (mask .== 1)
            ys[idx] .= (hs ./ 2 .+ R.ymin)[idx]
        end
        let idx = (ys .+ hs ./ 2 .> R.ymax) .& (mask .== 1)
            ys[idx] .= (hs ./ 2 .- R.ymax)[idx]
        end
        if vis visualize(xs, ys, ws, hs, R) end
    end
    # if vis visualize_density((xs, ys, ws, hs), R) end
    xs, ys
end

function visualize_density(vs, R)
    xs, ys, ws, hs = vs
    visualize(xs, ys, ws, hs, R)
    rho = rho_fast(gpu.(vs), R)
    phi, Ephix, Ephiy = phi_b(rho, R)
    # display_plot(Plots.heatmap(rho))
    display_plot(Plots.heatmap(reverse(rho, dims=1)))
    # display_plot(Plots.heatmap(permutedims(rho)))
    display_plot(Plots.heatmap(reverse(Ephix, dims=1)))
    display_plot(Plots.heatmap(reverse(Ephiy, dims=1)))
    # display_plot(Plots.bar(sort(bxs)))
end

function greedy_legalization(xs, ys, ws, hs, mask)
    # A Tetris-like greedy algorithm. It process items one by one, move to the
    # legal location. The legal for VLSI is (1) valid row/column grids (2)
    # non-overlapping. For us, the grid on PCB is mostly continuous, so we only
    # consider non-overlapping.

    # We have mixed-size components. Moving larger ones is harder. Thus should
    # we consider move them first?

    for i in 1:length(xs)
        if mask[i] == 0: continue end
        # 1. check if this cell overlaps with others. FIXME this sounds costly
        #
        # 2. if overlap, find the closest empty spot. FIXME how to find those
        # spots? I probably generate random locations, and check?
    end

    return xs, ys
end

"""Calculate the number of overlap of the rectangle defined by (x,y,w,h) and all
the rectangles.

fasts but also deprecated because it is not easy to support angle.

"""
function cost_f_fast(xs, ys, ws, hs, x, y, w, h, R)
    x1 = x - w/2
    y1 = y - h/2
    x2 = x + w/2
    y2 = y + h/2

    x1s = xs .- ws ./ 2
    y1s = ys .- hs ./ 2

    x2s = xs .+ ws ./ 2
    y2s = ys .+ hs ./ 2

    return sum((x1s .< x2) .& (y1s .< y2) .& (x2s .> x1) .& (y2s .> y1))
end

function cost_f(xs, ys, as, ws, hs, x, y, a, w, h, R; except=[])
    # scale everything down
    scale = max(R.xmax - R.xmin, R.ymax - R.ymin)
    xs = (xs .- R.xmin) ./ scale .+ 1
    ys = (ys .- R.ymin) ./ scale .+ 1
    ws = ws ./ scale
    hs = hs ./ scale

    x = (x - R.xmin) / scale + 1
    y = (y - R.ymin) / scale + 1
    w = w / scale
    h = h / scale

    # FIXME why would it happen, should the value be exact, and whether [1,2] is
    # still needed
    if x < 0.99 || x > 2.01
        # FIXME magic number
        @warn "x out of range: $x"
        return 100
    end

    l1, l2, l3, l4 = four_corners(x, y, a, w, h)

    # check for all the other rectangles
    ct = 0
    for i in 1:length(xs)
        if i in except continue end
        m1, m2, m3, m4 = four_corners(xs[i], ys[i], as[i], ws[i], hs[i])
        # m1, m2, m3, m4 = four_corners(xs[i], ys[i], ws[i], hs[i])
        # FIXME the four corners of the polygon will never be in polygon
        #
        # FIXME of course this is incorrect. It might be the case where one
        # rectangle is inside another. Thus the corners of the larger one will
        # never be inside the small one.
        #
        # maybe I'm doing double direction check
        if overlap(l1, l2, l3, l4, m1, m2, m3, m4)
            ct += 1
        end
    end
    return ct
end

function overlap(l1, l2, l3, l4, r1, r2, r3, r4)
    tri1 = Primitive(l1, l2, l3)
    tri2 = Primitive(l1, l4, l3)

    tri3 = Primitive(r1, r2, r3)
    tri4 = Primitive(r1, r4, r3)

    if intriangle(tri1, r1) > 0 ||
        intriangle(tri1, r2) > 0 ||
        intriangle(tri1, r3) > 0 ||
        intriangle(tri1, r4) > 0 ||

        intriangle(tri2, r1) > 0 ||
        intriangle(tri2, r2) > 0 ||
        intriangle(tri2, r3) > 0 ||
        intriangle(tri2, r4) > 0 ||

        intriangle(tri3, l1) > 0 ||
        intriangle(tri3, l2) > 0 ||
        intriangle(tri3, l3) > 0 ||
        intriangle(tri3, l4) > 0 ||

        intriangle(tri4, l1) > 0 ||
        intriangle(tri4, l2) > 0 ||
        intriangle(tri4, l3) > 0 ||
        intriangle(tri4, l4) > 0

        return true
    end
    return false
end

function test()
    overlap(four_corners(786, 617, 1.25, 122, 81)...,
            four_corners(707, 730, -0.314, 224, 81)...)
end

function four_corners(x, y, w, h)
    # compute the 4 corners of the rectangle
    l1 = GP.Point(x - w/2, y - h/2)
    l2 = GP.Point(x + w/2, y - h/2)
    l3 = GP.Point(x + w/2, y + h/2)
    l4 = GP.Point(x - w/2, y + h/2)
    return l1, l2, l3, l4
end

function four_corners(x, y, a, w, h)
    r = sqrt((h/2)^2 + (w/2)^2)
    return map([h/2, h/2, -h/2, -h/2],
               [w/2, -w/2, -w/2, w/2]) do Δh, Δw
                   # 1. compute theta
                   θ = atan(Δh, Δw)
                   x1 = x - cos(θ + a) * r
                   y1 = y + sin(θ + a) * r
                   GP.Point(x1, y1)
               end
end

function test()
    four_corners(xs[1], ys[1], as[1], ws[1], hs[1])
    four_corners(xs[1], ys[1], ws[1], hs[1])
    four_corners(xs[1], ys[1], 1.2, ws[1], hs[1])
    map([1,2,3], [4,5,6]) do x, y
        (x,y)
    end
end

function accept(xs, ys, as, ws, hs, x, y, a, i, t, R)
    # calculate the xs[i], ys[i] conflicts with how many others
    c0 = cost_f(xs, ys, as, ws, hs,
                xs[i], ys[i], as[i], ws[i], hs[i],
                R, except=[i])
    # FIXME I should remove the conflict with itself
    c1 = cost_f(xs, ys, as, ws, hs,
                x, y, a, ws[i], hs[i],
                R, except=[i])
    if c1 < c0
        # @info "cost improves from $c0 to $c1"
        return true
    end
    p = exp(-(c1 - c0) / t)
    # @show p
    if rand() < p
        return true
    else
        return false
    end
end

function temperature(step)
    1 / log(step)
end

"""number of conflicts of all components
FIXME fixed position?
TODO report and debug this if there are still conflicts left?
"""
function compute_conflicts(xs, ys, as, ws, hs, R)
    # conflicted items
    items = []
    for i in 1:length(xs)
        c = cost_f(xs, ys, as, ws, hs,
                   xs[i], ys[i], as[i], ws[i], hs[i],
                   R, except=[i])
        if c > 0
            append!(items, i)
        end
    end
    return items
end

function test()
    compute_conflicts(solxs2, solys2, solas, ws, hs, R)
end

function simulated_annealing_legalization(xs, ys, as, ws, hs, mask, diearea;
                                          vis=false, ncycles=20,
                                          nsteps=100, stepsize=50,
                                          theta_stepsize=0)
    # FIXME make a copy?
    xs = Float32.(xs)
    ys = Float32.(ys)
    ws = Float32.(ws)
    hs = Float32.(hs)
    as = Float32.(as)
    # FIXME this is for visulization and map back to valid region
    R = Region(xs, ys, ws, hs, diearea, 300)
    # actually I can probably parallelize the SA process, by trying to move
    # multiple components
    # t = 50
    # randomly choose a point
    for cycle in 2:ncycles+1
        t = temperature(cycle)
        for step in 1:nsteps
            # no movable components, not meaningful, for debugging purpose
            if isempty(findall(mask .== 1)) break end
            i = rand(findall(mask .== 1))
            if cost_f(xs, ys, as, ws, hs,
                      xs[i], ys[i], as[i], ws[i], hs[i],
                      R, except=[i]) == 0
                continue
            end
            # i = rand(1:length(xs))
            # FIXME the scale of the movement
            x = xs[i] + stepsize * randn()
            y = ys[i] + stepsize * randn()
            # FIXME maybe have a separate variable to control change angle or not?
            # FIXME what should be the step size? was 50 for x,y
            #
            # if theta_stepsize, this is effectively not changing angle
            a = as[i] + theta_stepsize * randn()

            # newxs = xs .+ randn()
            # newys = ys .+ randn()
            if accept(xs, ys, as, ws, hs,
                      x, y, a,
                      i, t, R)
                # @info "cycle $cycle step $step accepted"
                xs[i] = x
                ys[i] = y
                as[i] = a
            end
        end

        # map back to valid region
        # validate_region!(xs, ys, ws, hs, R)
        let idx = (xs .- ws ./ 2 .< R.xmin) .& (mask .== 1)
            xs[idx] .= (ws ./ 2 .+ R.xmin)[idx]
        end
        let idx = (xs .+ ws ./ 2 .> R.xmax) .& (mask .== 1)
            xs[idx] .= (ws ./ 2 .- R.xmax)[idx]
        end
        let idx = (ys .- hs ./ 2 .< R.ymin) .& (mask .== 1)
            ys[idx] .= (hs ./ 2 .+ R.ymin)[idx]
        end
        let idx = (ys .+ hs ./ 2 .> R.ymax) .& (mask .== 1)
            ys[idx] .= (hs ./ 2 .- R.ymax)[idx]
        end
        
        # print how many conflicts
        conflicts = compute_conflicts(xs, ys, as, ws, hs, R)
        @info "cycle $cycle, remaining conflicts: " length(conflicts)

        # break if already no conflicts
        if length(conflicts) == 0 break end
        # FIXME visualize the angle
        if vis visualize(xs, ys, ws, hs, R) end
    end
    conflicts = compute_conflicts(xs, ys, as, ws, hs, R)
    return xs, ys, as, conflicts
end

