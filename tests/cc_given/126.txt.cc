(program
 (define-env $env_t40 ($loop15 $seq14 k16))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t42 (k15))
 (define-env $env_t43 (k24))
 (define-env $env_t44 (i14))
 (define-env $env_t41 ($loop15 $seq14))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$y (void))
 (define g$x (void))
 (set-then!
  g$y
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
        (lambda ($env $seq14 $loop15 k15)
          (app*
           (make-closure
            (lambda ($env k16)
              ((cps set?)
               (env-ref $env_t41 $env $seq14)
               (make-closure
                (lambda ($env rv18)
                  (if rv18
                    (app*
                     for-set-k
                     (env-ref $env_t40 $env $seq14)
                     (env-ref $env_t40 $env $loop15)
                     (env-ref $env_t40 $env k16))
                    ((cps tuple?)
                     (env-ref $env_t40 $env $seq14)
                     (make-closure
                      (lambda ($env rv19)
                        (if rv19
                          (app*
                           for-tuple-k
                           (env-ref $env_t40 $env $seq14)
                           (env-ref $env_t40 $env $loop15)
                           (env-ref $env_t40 $env k16))
                          ((cps py-list?)
                           (env-ref $env_t40 $env $seq14)
                           (make-closure
                            (lambda ($env rv20)
                              (if rv20
                                (app*
                                 for-py-list-k
                                 (env-ref $env_t40 $env $seq14)
                                 (env-ref $env_t40 $env $loop15)
                                 (env-ref $env_t40 $env k16))
                                ((cps dict?)
                                 (env-ref $env_t40 $env $seq14)
                                 (make-closure
                                  (lambda ($env rv21)
                                    (if rv21
                                      (app*
                                       for-dict-k
                                       (env-ref $env_t40 $env $seq14)
                                       (env-ref $env_t40 $env $loop15)
                                       (env-ref $env_t40 $env k16))
                                      (app*
                                       (env-ref $env_t40 $env k16)
                                       (void))))
                                  (make-env
                                   $env_t40
                                   ($loop15 (env-ref $env_t40 $env $loop15))
                                   ($seq14 (env-ref $env_t40 $env $seq14))
                                   (k16 (env-ref $env_t40 $env k16)))))))
                            (make-env
                             $env_t40
                             ($loop15 (env-ref $env_t40 $env $loop15))
                             ($seq14 (env-ref $env_t40 $env $seq14))
                             (k16 (env-ref $env_t40 $env k16)))))))
                      (make-env
                       $env_t40
                       ($loop15 (env-ref $env_t40 $env $loop15))
                       ($seq14 (env-ref $env_t40 $env $seq14))
                       (k16 (env-ref $env_t40 $env k16)))))))
                (make-env
                 $env_t40
                 ($loop15 (env-ref $env_t41 $env $loop15))
                 ($seq14 (env-ref $env_t41 $env $seq14))
                 (k16 k16)))))
            (make-env $env_t41 ($loop15 $loop15) ($seq14 $seq14)))
           (make-closure
            (lambda ($env rv17) (app* (env-ref $env_t42 $env k15) (void)))
            (make-env $env_t42 (k15 k15)))))
        (make-env $env_t39))
       (py-list* 1 2 3)
       (make-closure
        (lambda ($env i14 k22)
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
            (lambda ($env continue k23)
              (set-then!
               g$x
               (env-ref $env_t44 $env i14)
               (app*
                (make-closure
                 (lambda ($env k24)
                   ((cps py-print)
                    g$x
                    (make-closure
                     (lambda ($env rv25)
                       ((cps +)
                        g$x
                        g$y
                        (make-closure
                         (lambda ($env rv26)
                           (set-then!
                            g$y
                            rv26
                            (app* (env-ref $env_t43 $env k24) (void))))
                         (make-env
                          $env_t43
                          (k24 (env-ref $env_t43 $env k24))))))
                     (make-env $env_t43 (k24 k24)))))
                 (make-env $env_t39))
                k23)))
            (make-env $env_t44 (i14 i14)))
           k22))
        (make-env $env_t39))
       k14))
    (make-env $env_t39))
   $halt)))
