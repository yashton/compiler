(program
 (define-env $env_t42 (loop))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k19))
 (define-env $env_t43 (k15))
 (define-env $env_t41 (k16 loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
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
    (lambda ($env break k14)
      (app*
       (make-closure
        (lambda ($env loop k15)
          (set-then!
           loop
           (make-cell loop)
           (set-cell!
            loop
            (make-closure
             (lambda ($env k16)
               ((cps >)
                g$x
                0
                (make-closure
                 (lambda ($env rv17)
                   (if rv17
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
                       (lambda ($env continue k18)
                         (app*
                          (make-closure
                           (lambda ($env k19)
                             ((cps -)
                              g$x
                              1
                              (make-closure
                               (lambda ($env rv20)
                                 (set-then!
                                  g$x
                                  rv20
                                  ((cps py-print)
                                   g$x
                                   (env-ref $env_t40 $env k19))))
                               (make-env $env_t40 (k19 k19)))))
                           (make-env $env_t39))
                          k18))
                       (make-env $env_t39))
                      (make-closure
                       (lambda ($env rv21)
                         (app*
                          (get-cell (env-ref $env_t41 $env loop))
                          (env-ref $env_t41 $env k16)))
                       (make-env
                        $env_t41
                        (k16 (env-ref $env_t41 $env k16))
                        (loop (env-ref $env_t41 $env loop)))))
                     (app* (env-ref $env_t41 $env k16) (void))))
                 (make-env
                  $env_t41
                  (k16 k16)
                  (loop (env-ref $env_t42 $env loop))))))
             (make-env $env_t42 (loop loop)))
            (app*
             (get-cell loop)
             (make-closure
              (lambda ($env rv22)
                (app*
                 (make-closure
                  (lambda ($env k23) ((cps py-print) "didn't run\n" k23))
                  (make-env $env_t39))
                 (env-ref $env_t43 $env k15)))
              (make-env $env_t43 (k15 k15)))))))
        (make-env $env_t39))
       (void)
       k14))
    (make-env $env_t39))
   $halt)))
