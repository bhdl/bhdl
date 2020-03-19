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
