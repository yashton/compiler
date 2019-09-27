(program
 (define-env $env_t43 (k16))
 (define-env $env_t41 (k17 loop))
 (define-env $env_t42 (loop))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k20))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  ((cps py-print)
   g$x
   (make-closure
    (lambda ($env rv14)
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
        (lambda ($env break k15)
          (app*
           (make-closure
            (lambda ($env loop k16)
              (set-then!
               loop
               (make-cell loop)
               (set-cell!
                loop
                (make-closure
                 (lambda ($env k17)
                   ((cps >)
                    g$x
                    0
                    (make-closure
                     (lambda ($env rv18)
                       (if rv18
                         (app*
                          (make-closure
                           (lambda ($env f cc)
                             (app*
                              f
                              (make-closure
                               (lambda ($env x k)
                                 (app* (env-ref $env_t38 $env cc) x))
                               (make-env $env_t38 (cc cc)))
                              cc))
                           (make-env $env_t39))
                          (make-closure
                           (lambda ($env continue k19)
                             (app*
                              (make-closure
                               (lambda ($env k20)
                                 ((cps py-print)
                                  g$x
                                  (make-closure
                                   (lambda ($env rv21)
                                     ((cps -)
                                      g$x
                                      1
                                      (make-closure
                                       (lambda ($env rv22)
                                         (set-then!
                                          g$x
                                          rv22
                                          ((cps py-print)
                                           g$x
                                           (env-ref $env_t40 $env k20))))
                                       (make-env
                                        $env_t40
                                        (k20 (env-ref $env_t40 $env k20))))))
                                   (make-env $env_t40 (k20 k20)))))
                               (make-env $env_t39))
                              k19))
                           (make-env $env_t39))
                          (make-closure
                           (lambda ($env rv23)
                             (app*
                              (get-cell (env-ref $env_t41 $env loop))
                              (env-ref $env_t41 $env k17)))
                           (make-env
                            $env_t41
                            (k17 (env-ref $env_t41 $env k17))
                            (loop (env-ref $env_t41 $env loop)))))
                         (app* (env-ref $env_t41 $env k17) (void))))
                     (make-env
                      $env_t41
                      (k17 k17)
                      (loop (env-ref $env_t42 $env loop))))))
                 (make-env $env_t42 (loop loop)))
                (app*
                 (get-cell loop)
                 (make-closure
                  (lambda ($env rv24)
                    (app*
                     (make-closure
                      (lambda ($env k25) ((cps py-print) "didn't run\n" k25))
                      (make-env $env_t39))
                     (env-ref $env_t43 $env k16)))
                  (make-env $env_t43 (k16 k16)))))))
            (make-env $env_t39))
           (void)
           k15))
        (make-env $env_t39))
       $halt))
    (make-env $env_t39)))))
