(program
 (define-label
  $lambda38
  (lambda ($env rv14) (set-then! g$x rv14 ((cps py-print) g$x $halt))))
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  30
  ((cps +)
   g$x
   20
   (make-closure (lambda-label $lambda38) (make-env $env_t38)))))
