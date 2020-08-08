# Advanced BHDL Reference

## the `bhdl/splicing` language

The language `bhdl/splicing` is not BHDL syntax. It provides a simple "splicing
syntax" for function application:

```lisp
#lang reader bhdl/splicing

(list 1 2 (list 3 4) .. 5)
;; equivalent to
(list 1 2 3 4 5)
;; which evaluates to => '(1 2 3 4 5)
```

## requiring `pict` library with prefix

Since `bhdl` re-exposes `*-append` and `*-superimpose` functions to be generic
to `bhdl` components and circuits, if you want to import `pict`, you need to
prefix it, e.g.:

```lisp
(require (prefix-in pict: pict))
```

## Auto Placement
We implemented an analytical global placer and a simulated annealing detailed
placer. To use them, follow the above installation guide to setup the julia
environment and run the server in the backend:

```
julia --project server.jl
```

You can then send the placement request to backend server via:
```lisp
(define place-result
    (send-for-placement
     (Composite->place-spec whole
                            #:place-nsteps 50
                            #:place-nbins 300
                            #:sa-ncycles 10
                            #:sa-nsteps 3000
                            #:sa-stepsize 10
                            #:sa-theta-stepsize 0.3)))
```

You can visualize the placed Composite via:

```lisp
(Composite->pict whole place-result)
```
