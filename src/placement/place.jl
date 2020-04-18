import FFTW
using SparseArrays: sparse, spzeros
import ForwardDiff
import Zygote
using Statistics: mean
import Plots
import CuArrays: cu, allowscalar
allowscalar(false)
using Flux: gpu, cpu

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
    res = spzeros(length(x))
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
    f = e->We_grad_x(e,x,xory)
    # res = spzeros(length(x))
    tmp = Array{Any}(nothing, length(nets))
    Threads.@threads for i in 1:length(nets)
        tmp[i] = f(nets[i])
    end
    sum(tmp)
end

# function W_grad(Es, xs, ys)
#     wx = W_grad_x(Es, xs, :x)
#     wy = W_grad_x(Es, ys, :y)
#     wx, wy
# end

function test()
    # export JULIA_NUM_THREADS=4
    Threads.nthreads()
    @time W_grad_x(Es, xs)
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
    x1 = max.(pxs .- pws / 2, cpu(R.bx1s))
    y1 = max.(pys .- phs / 2, cpu(R.by1s))
    x2 = min.(pxs .+ pws / 2, cpu(R.bx2s))
    y2 = min.(pys .+ phs / 2, cpu(R.by2s))

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
    pxs, pys, pws, phs = (pxs, pys, pws, phs) .|> gpu
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
    xs, ys, ws, hs = vs
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
    @time rrr = rho_cells(vs, R);
    size(rrr)
    size(phi)
    one = dropdims(sum(rrr .* phi, dims=(1,2)), dims=(1,2))
    two = dropdims(sum(rrr .* Ephix, dims=(1,2)), dims=(1,2))
    three = dropdims(sum(rrr .* Ephiy, dims=(1,2)), dims=(1,2))
    return one, two, three
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

# return a new pos
function place(xs, ys, ws, hs, Es, mask, diearea; vis=false)
    xs = Float32.(xs)
    ys = Float32.(ys)
    ws = Float32.(ws)
    hs = Float32.(hs)

    # first, devide into bins
    # FIXME use a more formal way of deciding the bouding box
    # FIXME more bins
    R = Region(xs, ys, ws, hs, diearea, 300)

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
    for step in 1:50
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
        d, dx, dy = density((xs, ys, ws, hs), R);
        dx[mask .== 0] .= 0
        dy[mask .== 0] .= 0
        # weights HP here
        dx .*= 0.001
        dy .*= 0.001
        wgradx .*= 0.01
        wgrady .*= 0.01

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

        @info "data" step loss mean(abs.(wgradx)) mean(abs.(dx)) hpwl(xs, ys, Es)

        # apply mask for fixed macros
        xs .-= deltax .* mask
        ys .-= deltay .* mask

        # map back to valid region
        # FIXME consider w and h
        xs[xs .< R.xmin] .= R.xmin
        xs[xs .> R.xmax] .= R.xmax
        ys[ys .< R.ymin] .= R.ymin
        ys[ys .> R.ymax] .= R.ymax
        if vis visualize(xs, ys, ws, hs, R) end
    end
    if vis visualize_density((xs, ys, ws, hs), R) end
    xs, ys
end

function visualize_density(vs, R)
    xs, ys, ws, hs = vs
    visualize(xs, ys, ws, hs, R)
    rho = rho_fast(vs, R)
    phi, Ephix, Ephiy = phi_b(rho, R)
    # display_plot(Plots.heatmap(rho))
    display_plot(Plots.heatmap(reverse(rho, dims=1)))
    # display_plot(Plots.heatmap(permutedims(rho)))
    display_plot(Plots.heatmap(reverse(Ephix, dims=1)))
    display_plot(Plots.heatmap(reverse(Ephiy, dims=1)))
    # display_plot(Plots.bar(sort(bxs)))
end

