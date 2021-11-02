
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPrmYHr6Ux6K racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide start-placement-engine stop-placement-engine output-placement-engine
    
    
    )

    (define the-placement-process (make-parameter #f))

(define (start-placement-engine [port 8082] [use-gpu #t])
  "Launch placement engine on port. If already running, do nothing."
  ;; check the engine running.
  ;; TOOD whether the engine should be shalled for other processes? Looks like yes, because we are using a port
  ;; TODO whether to launch different engines on different port?
  ;; TODO choose to use GPU or not
  ;; TODO this will be launched asynchronizely, and I need a way to monitor the output. This output is probably better shown in a separate window.
  (when (not (get-8082-processid))
         ;; TODO change directory temporarily
      (debug "Instantiating the julia project packages ..")
      (shell "cd ~/bhdl/placement && julia --project -e 'import Pkg; Pkg.instantiate(); Pkg.precompile();'")
      (debug "Starting the placement engine asynchronizely ..")
    ;;         (list stdout stdin id stderr func)
             ;; TODO how to redirect the stdout to a file?
        (the-placement-process (process "cd ~/bhdl/placement && julia --project main.jl > ~/bhdl/output.txt 2>&1"))
            ;; I actually wanted to wait a little bit until the process is listening on that port
            (wait-for-8082)
      ))

(define (get-8082-processid)
  ;; HACK I'm depending on the behavior that if the sting is not a number, string->number will return #f
  (string->number (string-trim (with-output-to-string (lambda () (shell "lsof -i :8082 | awk '{print $2}' | tail -n 1"))))))


(define (stop-8082-process)
  (let ([id (get-8082-processid)])
    (when id
          (shell (~a "kill -9 " id)))))

(define (wait-for-8082)
  (for ([i (in-naturals)]
      #:break (get-8082-processid))
     (displayln (~a "The TCP port is still " (get-8082-processid) ", wainting for 1 sec .."))
;;      (flush-output)
     (sleep 1))
  (display (~a "TCP port ready: " (get-8082-processid))))

(define (stop-placement-engine)
  "Close placement engine."
  ;; TODO I would also run a ps -ef | grep .. or a lsof of 8022 for any existing process
;;   (when (the-placement-process)
;;         ((list-ref (the-placement-process) 4) 'kill)
;;         (the-placement-process #f))
  ;; actually stop the process on 8082
  (stop-8082-process))

(define (output-placement-engine)
  (when (the-placement-process)
        ;; read the stdin
        ;; FIXME however this can read only once
        ;; FIXME this is blocking
        (port->string (list-ref (the-placement-process) 0)
                      #:close? #f)))


  )
    