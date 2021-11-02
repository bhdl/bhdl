import Random
using HTTP

function web_server()
    @info "Listening on localhost:8082 .."
    # FIXME 8081 seems to be used by jupyterhub, I'm thus using 8082
    HTTP.serve(HTTP.Sockets.localhost, 8082) do request::HTTP.Request
        @show request
        @show request.method

        payload = HTTP.payload(request)
        # CAUTION the String constructor method will clear the payload!
        # String(payload)

        @info "parsing payload .."
        jstr = String(payload)
        jobj = JSON.parse(jstr)
        xs, ys, as, ws, hs, Es, mask, diearea, params = decode_place_spec(jobj)
        @info "running placement .."

        # set seed. This should make the placement deterministic given the same
        # hyper-parameters
        Random.seed!(1234)

        solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea,
                             nsteps=params["place-nsteps"],
                             nbins=params["place-nbins"])
        solxs, solys, solas, conflicts = simulated_annealing_legalization(
            solxs, solys, as, ws, hs, mask, diearea,
            ncycles=params["sa-ncycles"],
            nsteps=params["sa-nsteps"],
            stepsize=params["sa-stepsize"],
            theta_stepsize=params["sa-theta-stepsize"])

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
        #
        # UPDATE I'm using center at every locations, to be consistent for fixed locations
        # solxs = solxs .- ws ./ 2
        # solys = solys .- hs ./ 2
        res_payload = Dict("xs"=>solxs, "ys"=>solys,
                           "as"=>solas,
                           # DEBUG
                           "ws"=>ws,
                           "hs"=>hs,
                           # should I add diearea here?
                           "diearea"=>diearea,
                           # for visualization and debug
                           "conflicts"=>conflicts) |> JSON.json

        # TODO I also want to send back visualizations UPDATE probably just send
        # back the coordinates and optionally meta data during the process. The
        # rendering can be done on the client side.

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