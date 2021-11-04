web_server

jstr="""
{"Es":[[[1,25.269081954866458,53.10790105768544],[4,11.028455259962904,3.4263161972700282],[17,2.965904958261868,3.1265135300089004]],[[10,2.965904958261868,3.1265135300089004],[1,4.282895246587535,28.695398152136487]],[[3,18.587765370189903,18.587765370189903],[1,4.282895246587535,42.4006629412166]],[[19,9.711464971637236,3.1265135300089004],[2,16.06085717470326,4.272188008471066]],[[1,11.563817165786345,4.282895246587535],[1,4.282895246587535,14.990133363056373],[15,2.965904958261868,3.1265135300089004],[14,2.965904958261868,3.1265135300089004],[1,45.82697913848662,4.282895246587535],[12,2.965904958261868,3.1265135300089004],[2,13.276975264421361,4.272188008471066],[13,2.965904958261868,3.1265135300089004],[3,18.587765370189903,7.709211443857564],[11,2.965904958261868,3.1265135300089004],[1,4.282895246587535,32.12171434940652]],[[5,2.965904958261868,3.1265135300089004],[1,53.10790105768544,18.416449560326406]],[[16,2.965904958261868,3.1265135300089004],[4,3.5333885784347165,9.422369542492579],[1,28.695398152136487,53.10790105768544]],[[19,2.965904958261868,3.1265135300089004],[1,4.282895246587535,18.416449560326406]],[[16,9.711464971637236,3.1265135300089004],[3,18.587765370189903,29.46631929652224],[13,9.711464971637236,3.1265135300089004],[1,14.990133363056373,4.282895246587535],[9,2.2485155074176655,11.23809013359732],[4,3.5333885784347165,3.4263161972700282],[12,9.711464971637236,3.1265135300089004],[2,24.41250290554895,4.272188008471066],[17,9.711464971637236,3.1265135300089004],[1,4.282895246587535,25.269081954866458],[6,3.854054085020725,3.212436117866633],[10,9.711464971637236,3.1265135300089004],[15,9.711464971637236,3.1265135300089004],[14,9.711464971637236,3.1265135300089004],[11,9.711464971637236,3.1265135300089004]],[[9,2.2485155074176655,1.8161745780010006],[8,2.965904958261868,3.1265135300089004],[7,18.20270353554344,3.5385751645783343],[3,7.709211443857564,29.46631929652224],[1,14.990133363056373,53.10790105768544]],[[6,11.135070227914916,3.212436117866633],[5,9.711464971637236,3.1265135300089004]],[[3,7.709211443857564,18.587765370189903],[1,4.282895246587535,38.97434674394657]],[[18,9.711464971637236,3.1265135300089004],[2,18.844739084985157,4.272188008471066]],[[3,7.709211443857564,7.709211443857564],[1,4.282895246587535,45.82697913848662]],[[8,9.711464971637236,3.1265135300089004],[7,3.083689288728353,3.5385751645783343]],[[18,2.965904958261868,3.1265135300089004],[1,4.282895246587535,21.84276575759643]]],"as":[0,-3.141592653589793,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"conflicts":[],"diearea":[157.39079630427298,220.56350119143912],"hs":[57.819085828931726,26.55395052884272,37.47533340764093,13.276975264421358,6.681316584676554,6.8529450458930565,7.505439853815422,6.681316584676554,13.592971558609834,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554,6.681316584676554],"mask":[0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],"place-params":{"place-nbins":300,"place-nsteps":50,"sa-ncycles":10,"sa-nsteps":2000,"sa-stepsize":10,"sa-theta-stepsize":0},"ws":[57.819085828931726,38.117767694629066,26.768095291172095,14.990133363056373,13.105659454557857,14.561925213407303,21.714682348931035,13.105659454557857,22.91412450846362,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857,13.105659454557857],"xs":[78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649,78.69539815213649],"ys":[104.82105915632044,13.062830502091998,202.03997924994803,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956,110.28175059571956]}
"""

jstr = open(f->read(f, String), "spec.json")
nothing

jobj = JSON.parse(jstr)
xs, ys, as, ws, hs, Es, mask, diearea, params = decode_place_spec(jobj)
nothing

solxs, solys = place(xs, ys, ws, hs, Es, mask, diearea,
                        nsteps=params["place-nsteps"],
                        nbins=params["place-nbins"])
nothing

simulated_annealing_legalization(
            solxs, solys, as, ws, hs, mask, diearea,
            ncycles=params["sa-ncycles"],
            nsteps=params["sa-nsteps"],
            stepsize=params["sa-stepsize"],
            theta_stepsize=params["sa-theta-stepsize"])