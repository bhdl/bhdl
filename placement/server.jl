include("bench.jl")
include("visualize.jl")
include("place.jl")

using HTTP

# FIXME the documentation of HTTP is not very good. There are lots of
# undocumented APIs, like HTTP.payload

# test this: accept a file, append hello to the file, and respond back

function web_server()
    # payload = nothing
    # myreq = nothing
    # HTTP.target(myreq)
    HTTP.serve(HTTP.Sockets.localhost, 8081) do request::HTTP.Request
        # global myreq
        # myreq = request
        @show request
        @show request.method
        # @show HTTP.header(request, "Content-Type")
        # @show HTTP.payload(request)
        # @show typeof(HTTP.payload(request))

        # global payload
        payload = HTTP.payload(request)
        # @show payload
        # typeof(payload)
        # FIXME the String constructor method will clear the payload!
        # String(payload)

        @info "parsing payload .."
        jstr = String(payload)
        jobj = JSON.parse(jstr)
        xs, ys, ws, hs, Es, mask = parse_jobj(jobj)
        @info "running placement .."
        solxs, solys = place(xs, ys, ws, hs, Es, mask, vis=true)
        # FIXME region, die area
        # FIXME performance
        # FIXME run iterations
        @info "visualizing .."
        R = Region(xs, ys, ws, hs, 300)
        # TODO for each incoming request, plot the process and save as log
        visualize(xs, ys, ws, hs, R)
        visualize(solxs, solys, ws, hs, R)
        @info "sending results back .."
        # I'll only send back cells, or cell locations
        res_payload = encode(jobj, solxs, solys)

        # parse this directly as json, and save data to internal data structure
        # and start placement and routing.
        try
            # return HTTP.Response("Hello")
            return HTTP.Response(res_payload)
        catch e
            return HTTP.Response(404, "Error: $e")
        end
        @info "Done. Ready to accept next request."
    end
end

function test()
    # read json directly for debugging
    str = open("../out/a.json") do io
        read(io, String)
    end
    jobj = JSON.parse(str)

    xs, ys, ws, hs, Es, mask = parse_jobj(jobj)
    Profile.@profile
    @time solx,soly = place(xs, ys, ws, hs, Es, mask, vis=false)

    visualize(xs, ys, ws, hs, R)
    visualize(solxs, solys, ws, hs, R)

    # this is json output
    res = encode(jobj, solxs, solys)
end

function main()
    @info "starting a test run to warm the model up .."
    test()
    web_server()
end

main()
