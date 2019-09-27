(program
 (define-env $env_t47 (k15))
 (define-env $env_t40 (k25))
 (define-env $env_t38 (cc))
 (define-env $env_t41 (continue k27))
 (define-env $env_t42 (continue))
 (define-env $env_t43 (continue k20))
 (define-env $env_t39 ())
 (define-env $env_t44 (k19))
 (define-env $env_t45 (k16 loop))
 (define-env $env_t46 (loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$sum (void))
 (set-then!
  g$x
  0
  (set-then!
   g$sum
   0
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
                ((cps <=)
                 g$x
                 20
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
                              (app*
                               (make-closure
                                (lambda ($env k20)
                                  ((cps modulo)
                                   g$x
                                   2
                                   (make-closure
                                    (lambda ($env rv23)
                                      ((cps equal?)
                                       rv23
                                       0
                                       (make-closure
                                        (lambda ($env rv24)
                                          (if rv24
                                            (app*
                                             (make-closure
                                              (lambda ($env k25)
                                                ((cps +)
                                                 g$sum
                                                 g$x
                                                 (make-closure
                                                  (lambda ($env rv26)
                                                    (set-then!
                                                     g$sum
                                                     rv26
                                                     (app*
                                                      (env-ref
                                                       $env_t40
                                                       $env
                                                       k25)
                                                      (void))))
                                                  (make-env
                                                   $env_t40
                                                   (k25 k25)))))
                                              (make-env $env_t39))
                                             (env-ref $env_t43 $env k20))
                                            (app*
                                             (make-closure
                                              (lambda ($env k27)
                                                ((cps +)
                                                 g$x
                                                 1
                                                 (make-closure
                                                  (lambda ($env rv28)
                                                    (set-then!
                                                     g$x
                                                     rv28
                                                     (app*
                                                      (env-ref
                                                       $env_t41
                                                       $env
                                                       continue)
                                                      (env-ref
                                                       $env_t41
                                                       $env
                                                       k27))))
                                                  (make-env
                                                   $env_t41
                                                   (continue
                                                    (env-ref
                                                     $env_t42
                                                     $env
                                                     continue))
                                                   (k27 k27)))))
                                              (make-env
                                               $env_t42
                                               (continue
                                                (env-ref
                                                 $env_t43
                                                 $env
                                                 continue))))
                                             (env-ref $env_t43 $env k20))))
                                        (make-env
                                         $env_t43
                                         (continue
                                          (env-ref $env_t43 $env continue))
                                         (k20 (env-ref $env_t43 $env k20))))))
                                    (make-env
                                     $env_t43
                                     (continue
                                      (env-ref $env_t42 $env continue))
                                     (k20 k20)))))
                                (make-env
                                 $env_t42
                                 (continue (env-ref $env_t42 $env continue))))
                               (make-closure
                                (lambda ($env rv21)
                                  ((cps +)
                                   g$x
                                   1
                                   (make-closure
                                    (lambda ($env rv22)
                                      (set-then!
                                       g$x
                                       rv22
                                       (app*
                                        (env-ref $env_t44 $env k19)
                                        (void))))
                                    (make-env
                                     $env_t44
                                     (k19 (env-ref $env_t44 $env k19))))))
                                (make-env $env_t44 (k19 k19)))))
                            (make-env $env_t42 (continue continue)))
                           k18))
                        (make-env $env_t39))
                       (make-closure
                        (lambda ($env rv29)
                          (app*
                           (get-cell (env-ref $env_t45 $env loop))
                           (env-ref $env_t45 $env k16)))
                        (make-env
                         $env_t45
                         (k16 (env-ref $env_t45 $env k16))
                         (loop (env-ref $env_t45 $env loop)))))
                      (app* (env-ref $env_t45 $env k16) (void))))
                  (make-env
                   $env_t45
                   (k16 k16)
                   (loop (env-ref $env_t46 $env loop))))))
              (make-env $env_t46 (loop loop)))
             (app*
              (get-cell loop)
              (make-closure
               (lambda ($env rv30)
                 (app*
                  (make-closure
                   (lambda ($env k31) ((cps py-print) "didn't run\n" k31))
                   (make-env $env_t39))
                  (env-ref $env_t47 $env k15)))
               (make-env $env_t47 (k15 k15)))))))
         (make-env $env_t39))
        (void)
        k14))
     (make-env $env_t39))
    (make-closure
     (lambda ($env rv32) ((cps py-print) g$sum $halt))
     (make-env $env_t39))))))
