(program
 (define-label $lambda38 (lambda ($env k15) (app* k15 1)))
 (define-label $lambda39 (lambda ($env k16) (app* k16 2)))
 (define-label
  $lambda40
  (lambda ($env rv14)
    (if rv14
      (app* (make-closure (lambda-label $lambda38) (make-env $env_t38)) $halt)
      (app*
       (make-closure (lambda-label $lambda39) (make-env $env_t38))
       $halt))))
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps >) 1 0 (make-closure (lambda-label $lambda40) (make-env $env_t38))))
