import FFTW
using SparseArrays: sparse, spzeros

function sumexp(x, γ)
    aP = exp.((x .- maximum(x)) / γ)
    aN = exp.(- (x .- minimum(x)) / γ)
    # This is not stable with large x
    # wex = sum(x .* exp.(x / γ)) / sum(exp.(x / γ)) - sum(x .* exp.(- x / γ)) / sum(exp.(- x / γ))
    sum(x .* aP) / sum(aP) - sum(x .* aN) / sum(aN)
end

function We(net, xs, ys)
    γ = 1.5
    x = [xs[n] for n in net]
    y = [ys[n] for n in net]
    wex = sumexp(x, γ)
    wey = sumexp(y, γ)
    return wex + wey
end

function W(nets, xs, ys)
    f = (e)->We(e, xs, ys)
    sum(f.(nets))
end

function We_grad_x_impl(net, x)
    γ = 1.5
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

function We_grad_x(net, x)
    res = spzeros(length(x))
    x = [x[n] for n in net]
    grad = We_grad_x_impl(net, x)
    # FIXME map back to whole x
    for (i,n) in enumerate(net)
        res[n] = grad[i]
    end
    res
end

function W_grad_x(nets, x)
    # batched by default
    batch_size = 10000
    ct = Int(ceil(length(nets) / batch_size))
    f = e->We_grad_x(e,x)
    res = spzeros(length(x))
    @showprogress 0.1 "batching .." for i in 1:ct
        start = (i-1) * batch_size + 1
        stop = min(i * batch_size, length(nets))
        # @show i start stop
        res .+= sum(f.(nets[start:stop]))
    end
    res
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

function phi_b(vs, R)
    rho = rho_all(vs, R)
    auv = FFTW.dct(rho)

    wuv_f(uv) = 2 * pi * uv / R.M
    wuv2 = reshape([wuv_f(u)^2 + wuv_f(v)^2 for u in 1:R.M for v in 1:R.M], R.M, R.M)
    wu = reshape([wuv_f(u) for u in 1:R.M for v in 1:R.M], R.M, R.M)
    wv = reshape([wuv_f(v) for u in 1:R.M for v in 1:R.M], R.M, R.M)

    # R.M,R.M matrix
    phi = FFTW.idct(auv ./ wuv2)
    Ephix = FFTW.idct(auv .* wv ./ wuv2)
    Ephiy = FFTW.idct(auv .* wu ./ wuv2)
    phi, Ephix, Ephiy
end

function density(vs, R)
    xs, ys, ws, hs = vs
    # gradient
    phi, Ephix, Ephiy = phi_b(vs, R)
    # calculate for each v

    # map x,y into bin
    bxs = Int.(floor.((xs .- R.xmin) ./ R.bw)) .+ 1
    bys = Int.(floor.((ys .- R.ymin) ./ R.bh)) .+ 1
    # calculate phi
    phis = ((bx,by)->phi[bx,by]).(bxs, bys)
    Ephixs = ((bx,by)->Ephix[bx,by]).(bxs, bys)
    Ephiys = ((bx,by)->Ephiy[bx,by]).(bxs, bys)
    # qi * phis
    qis = ws .* hs
    return qis .* phis, qis .* Ephixs, qis .* Ephiys
end

struct Region
    M
    bw
    bh
    xmin
    xmax
    ymin
    ymax
end

function Region(xs, ys, ws, hs, M)
    xmin = minimum(xs-ws/2)
    xmax = maximum(xs+ws/2)
    ymin = minimum(ys-hs/2)
    ymax = maximum(ys+hs/2)

    # M = 10
    bw = (xmax-xmin) / M + 1e-8
    bh = (ymax-ymin) / M + 1e-8

    Region(M, bw, bh, xmin, xmax, ymin, ymax)
end

function hpwl(xs, ys, Es)
    res = map(Es) do e
        # max delta x
        abs(maximum((i->xs[i]).(e)) - minimum((i->xs[i]).(e))) +
            abs(maximum((i->ys[i]).(e)) - minimum((i->ys[i]).(e)))
    end
    sum(res)
end

# return a new pos
function place(xs, ys, ws, hs, Es, mask)
    xs = Float64.(xs)
    ys = Float64.(ys)

    # first, devide into bins
    # FIXME use a more formal way of deciding the bouding box
    # FIXME more bins
    R = Region(xs, ys, ws, hs, 10)

    # loss: HPWL and density penalty
    hpwl(xs, ys, Es)
    mask

    # iteratively solve the loss
    for step in 1:50
        @info "step: $step"
        w = W(Es, xs, ys)
        wgradx = W_grad_x(Es, xs)
        wgrady = W_grad_x(Es, ys)
        # remove the grad for fixed values

        # FIXME this density impl seems to be wrong
        d, dx, dy = density((xs, ys, ws, hs), R)
        deltax = wgradx .+ 0.1 * dx
        deltay = wgrady .+ 0.1 * dy
        loss = w + sum(d)

        # DEBUG use only grad
        # deltax = wgradx
        # deltay = wgrady
        # loss = w

        @info "data" step loss sum(d) hpwl(xs, ys, Es)

        # apply mask for fixed macros
        # FIXME this learning rate is huge
        xs .-= 100 * deltax .* mask
        ys .-= 100 * deltay .* mask
        
        # map back to valid region
        # FIXME consider w and h
        xs[xs .< R.xmin] .= R.xmin
        xs[xs .> R.xmax] .= R.xmax
        ys[ys .< R.ymin] .= R.ymin
        ys[ys .> R.ymax] .= R.ymax
        visualize(xs, ys, ws, hs)
    end
    xs, ys
end
