(program
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$b (void))
 (set-then!
  g$b
  5
  ((cps <)
   6
   g$b
   (make-closure
    (lambda ($env rv14) (set-then! g$a rv14 (app* $halt (void))))
    (make-env $env_t38)))))
