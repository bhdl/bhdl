include("bench.jl")
include("visualize.jl")
include("place.jl")

using HTTP

# FIXME the documentation of HTTP is not very good. There are lots of
# undocumented APIs, like HTTP.payload

# test this: accept a file, append hello to the file, and respond back

function web_server()
    HTTP.serve(HTTP.Sockets.localhost, 8081) do request::HTTP.Request
        @show request
        @show request.method

        payload = HTTP.payload(request)
        # CAUTION the String constructor method will clear the payload!
        # String(payload)

        @info "parsing payload .."
        jstr = String(payload)
        jobj = JSON.parse(jstr)
        xs, ys, ws, hs, Es, mask, diearea = decode_place_spec(jobj)
        @info "running placement .."
        solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea, vis=false)
        # FIXME run iterations

        # @info "visualizing .."
        # R = Region(xs, ys, ws, hs, 300)
        # TODO for each incoming request, plot the process and save as log
        # visualize(xs, ys, ws, hs, R)
        # visualize(solxs, solys, ws, hs, R)

        @info "sending results back .."
        # I'll only send back cells, or cell locations
        # res_payload = encode(jobj, solxs, solys)
        #
        # the new payload just encode the xs and ys
        #
        # transform the result to corner based, because racket pict system convention
        solxs = solxs .- ws ./ 2
        solys = solys .- hs ./ 2
        res_payload = Dict("xs"=>solxs, "ys"=>solys) |> JSON.json

        # parse this directly as json, and save data to internal data structure
        # and start placement and routing.
        try
            # return HTTP.Response("Hello")
            return HTTP.Response(res_payload)
        catch e
            return HTTP.Response(404, "Error: $e")
        end
    end
end

function test()
    # read json directly for debugging
    #
    # FIXME relative path might not work
    str = open("../../tests/warmup.json") do io
        read(io, String)
    end
    jobj = JSON.parse(str)

    xs, ys, ws, hs, Es, mask, diearea = decode_place_spec(jobj)

    # place(xs, ys, ws, hs, Es, mask, diearea, vis=true)

    @time solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea, vis=false)

    # visualize(xs, ys, ws, hs, R)
    # visualize(solxs, solys, ws, hs, R)
end

function main()
    @info "starting a test run to warm the model up .."
    test()
    @info "starting server .."
    web_server()
end

main()
