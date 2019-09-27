(program
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps >)
  1
  0
  (make-closure
   (lambda ($env rv14)
     (if rv14
       (app*
        (make-closure (lambda ($env k15) (app* k15 1)) (make-env $env_t38))
        $halt)
       (app*
        (make-closure (lambda ($env k16) (app* k16 2)) (make-env $env_t38))
        $halt)))
   (make-env $env_t38))))
