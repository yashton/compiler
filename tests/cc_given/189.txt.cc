(program
 (define-env $env_t40 (called k19))
 (define-env $env_t41 (called))
 (define-env $env_t44 (k28))
 (define-env $env_t45 (func k25))
 (define-env $env_t42 (called k16))
 (define-env $env_t43 (called func k16))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$outfunc2 (void))
 (define g$called (void))
 (define g$outfunc (void))
 (set-then!
  g$outfunc
  (make-closure
   (lambda ($env k14)
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
           (lambda ($env called func k16)
             (set-then!
              called
              (make-cell called)
              (set-then!
               func
               (make-cell func)
               (set-cell!
                called
                0
                (set-cell!
                 func
                 (make-closure
                  (lambda ($env k17)
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
                      (lambda ($env return k18)
                        (app*
                         (make-closure
                          (lambda ($env k19)
                            ((cps +)
                             (get-cell (env-ref $env_t41 $env called))
                             1
                             (make-closure
                              (lambda ($env rv20)
                                (set-cell!
                                 (env-ref $env_t40 $env called)
                                 rv20
                                 (app* (env-ref $env_t40 $env k19) (void))))
                              (make-env
                               $env_t40
                               (called (env-ref $env_t41 $env called))
                               (k19 k19)))))
                          (make-env
                           $env_t41
                           (called (env-ref $env_t41 $env called))))
                         k18))
                      (make-env
                       $env_t41
                       (called (env-ref $env_t41 $env called))))
                     k17))
                  (make-env $env_t41 (called called)))
                 (app*
                  (get-cell func)
                  (make-closure
                   (lambda ($env rv21)
                     (app*
                      (get-cell (env-ref $env_t43 $env func))
                      (make-closure
                       (lambda ($env rv22)
                         ((cps py-print)
                          (get-cell (env-ref $env_t42 $env called))
                          (env-ref $env_t42 $env k16)))
                       (make-env
                        $env_t42
                        (called (env-ref $env_t43 $env called))
                        (k16 (env-ref $env_t43 $env k16))))))
                   (make-env
                    $env_t43
                    (called called)
                    (func func)
                    (k16 k16)))))))))
           (make-env $env_t39))
          (void)
          (void)
          k15))
       (make-env $env_t39))
      k14))
   (make-env $env_t39))
  (set-then!
   g$called
   0
   (set-then!
    g$outfunc2
    (make-closure
     (lambda ($env k23)
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
         (lambda ($env return k24)
           (app*
            (make-closure
             (lambda ($env func k25)
               (set-then!
                func
                (make-cell func)
                (set-cell!
                 func
                 (make-closure
                  (lambda ($env k26)
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
                      (lambda ($env return k27)
                        (app*
                         (make-closure
                          (lambda ($env k28)
                            ((cps +)
                             g$called
                             1
                             (make-closure
                              (lambda ($env rv29)
                                (set-then!
                                 g$called
                                 rv29
                                 (app* (env-ref $env_t44 $env k28) (void))))
                              (make-env $env_t44 (k28 k28)))))
                          (make-env $env_t39))
                         k27))
                      (make-env $env_t39))
                     k26))
                  (make-env $env_t39))
                 (app*
                  (get-cell func)
                  (make-closure
                   (lambda ($env rv30)
                     (app*
                      (get-cell (env-ref $env_t45 $env func))
                      (env-ref $env_t45 $env k25)))
                   (make-env $env_t45 (func func) (k25 k25)))))))
             (make-env $env_t39))
            (void)
            k24))
         (make-env $env_t39))
        k23))
     (make-env $env_t39))
    (app*
     g$outfunc
     (make-closure
      (lambda ($env rv31)
        (app*
         g$outfunc2
         (make-closure
          (lambda ($env rv32) ((cps py-print) g$called $halt))
          (make-env $env_t39))))
      (make-env $env_t39)))))))
