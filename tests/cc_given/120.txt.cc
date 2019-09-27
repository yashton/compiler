(program
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
  (make-closure
   (lambda ($env x k14)
     (app*
      (make-closure
       (lambda ($env f cc)
         (app*
          f
          (make-closure
           (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x))
           (make-env $env_t38 (cc cc)))
          cc))
       (make-env $env_t39))
      (make-closure
       (lambda ($env return k15) (app* return (env-ref $env_t40 $env x) k15))
       (make-env $env_t40 (x x)))
      k14))
   (make-env $env_t39))
  (app*
   g$f
   3
   (make-closure
    (lambda ($env rv16) ((cps py-print) rv16 $halt))
    (make-env $env_t39)))))
