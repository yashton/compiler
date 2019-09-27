(program
 (define-label
  $lambda38
  (lambda ($env rv17) (set-then! g$s rv17 (app* $halt (void)))))
 (define-label
  $lambda39
  (lambda ($env rv16)
    ((cps +)
     1
     0+2.0i
     (make-closure (lambda-label $lambda38) (make-env $env_t38)))))
 (define-label
  $lambda40
  (lambda ($env rv15)
    (app* rv15 (make-closure (lambda-label $lambda39) (make-env $env_t38)))))
 (define-label
  $lambda41
  (lambda ($env rv14)
    (app*
     get-field
     g$b
     fg
     (make-closure (lambda-label $lambda40) (make-env $env_t38)))))
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$s (void))
 (app*
  get-field
  g$a
  f
  (make-closure (lambda-label $lambda41) (make-env $env_t38))))
