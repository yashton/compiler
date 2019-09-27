(program
 (define-env $env_t40 (a k16))
 (define-env $env_t41 (a))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (make-closure
   (lambda ($env a b k14)
     (set-then!
      a
      (make-cell a)
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
        (lambda ($env return k15)
          (app*
           (make-closure
            (lambda ($env k16)
              ((cps py-print)
               (get-cell (env-ref $env_t41 $env a))
               (make-closure
                (lambda ($env rv17)
                  (set-cell!
                   (env-ref $env_t40 $env a)
                   3
                   ((cps py-print)
                    (get-cell (env-ref $env_t40 $env a))
                    (env-ref $env_t40 $env k16))))
                (make-env $env_t40 (a (env-ref $env_t41 $env a)) (k16 k16)))))
            (make-env $env_t41 (a (env-ref $env_t41 $env a))))
           k15))
        (make-env $env_t41 (a a)))
       k14)))
   (make-env $env_t39))
  (app* g$f 1 2 $halt)))
