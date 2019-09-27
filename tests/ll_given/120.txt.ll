(program
 (define-label
  $lambda38
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda39
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda38) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda40
  (lambda ($env return k15) (app* return (env-ref $env_t40 $env x) k15)))
 (define-label
  $lambda41
  (lambda ($env x k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda40) (make-env $env_t40 (x x)))
     k14)))
 (define-label $lambda42 (lambda ($env rv16) ((cps py-print) rv16 $halt)))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (x))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (make-closure (lambda-label $lambda41) (make-env $env_t39))
  (app* g$f 3 (make-closure (lambda-label $lambda42) (make-env $env_t39)))))
