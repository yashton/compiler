(program
 (define-env $env_t41 (e18 i17 k22))
 (define-env $env_t42 (e18))
 (define-env $env_t43 (k14))
 (define-env $env_t44 (k14 t14))
 (define-env $env_t38 (e16 i15 k16))
 (define-env $env_t39 (e16))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$m (void))
 (define g$s (void))
 (app*
  (make-closure
   (lambda ($env t14 k14)
     (app*
      (make-closure
       (lambda ($env e16 k15)
         (app*
          (make-closure
           (lambda ($env i15 k16)
             ((cps py-list?)
              (env-ref $env_t39 $env e16)
              (make-closure
               (lambda ($env rv17)
                 (if rv17
                   ((cps py-list-ref)
                    (env-ref $env_t38 $env e16)
                    (env-ref $env_t38 $env i15)
                    (env-ref $env_t38 $env k16))
                   ((cps tuple?)
                    (env-ref $env_t38 $env e16)
                    (make-closure
                     (lambda ($env rv18)
                       (if rv18
                         ((cps tuple-ref)
                          (env-ref $env_t38 $env e16)
                          (env-ref $env_t38 $env i15)
                          (env-ref $env_t38 $env k16))
                         ((cps dict?)
                          (env-ref $env_t38 $env e16)
                          (make-closure
                           (lambda ($env rv19)
                             (if rv19
                               ((cps dict-ref)
                                (env-ref $env_t38 $env e16)
                                (env-ref $env_t38 $env i15)
                                (env-ref $env_t38 $env k16))
                               (error
                                "cannot index object"
                                (env-ref $env_t38 $env k16))))
                           (make-env
                            $env_t38
                            (e16 (env-ref $env_t38 $env e16))
                            (i15 (env-ref $env_t38 $env i15))
                            (k16 (env-ref $env_t38 $env k16)))))))
                     (make-env
                      $env_t38
                      (e16 (env-ref $env_t38 $env e16))
                      (i15 (env-ref $env_t38 $env i15))
                      (k16 (env-ref $env_t38 $env k16)))))))
               (make-env
                $env_t38
                (e16 (env-ref $env_t39 $env e16))
                (i15 i15)
                (k16 k16)))))
           (make-env $env_t39 (e16 e16)))
          0
          k15))
       (make-env $env_t40))
      t14
      (make-closure
       (lambda ($env rv20)
         (set-then!
          g$m
          rv20
          (app*
           (make-closure
            (lambda ($env e18 k21)
              (app*
               (make-closure
                (lambda ($env i17 k22)
                  ((cps py-list?)
                   (env-ref $env_t42 $env e18)
                   (make-closure
                    (lambda ($env rv23)
                      (if rv23
                        ((cps py-list-ref)
                         (env-ref $env_t41 $env e18)
                         (env-ref $env_t41 $env i17)
                         (env-ref $env_t41 $env k22))
                        ((cps tuple?)
                         (env-ref $env_t41 $env e18)
                         (make-closure
                          (lambda ($env rv24)
                            (if rv24
                              ((cps tuple-ref)
                               (env-ref $env_t41 $env e18)
                               (env-ref $env_t41 $env i17)
                               (env-ref $env_t41 $env k22))
                              ((cps dict?)
                               (env-ref $env_t41 $env e18)
                               (make-closure
                                (lambda ($env rv25)
                                  (if rv25
                                    ((cps dict-ref)
                                     (env-ref $env_t41 $env e18)
                                     (env-ref $env_t41 $env i17)
                                     (env-ref $env_t41 $env k22))
                                    (error
                                     "cannot index object"
                                     (env-ref $env_t41 $env k22))))
                                (make-env
                                 $env_t41
                                 (e18 (env-ref $env_t41 $env e18))
                                 (i17 (env-ref $env_t41 $env i17))
                                 (k22 (env-ref $env_t41 $env k22)))))))
                          (make-env
                           $env_t41
                           (e18 (env-ref $env_t41 $env e18))
                           (i17 (env-ref $env_t41 $env i17))
                           (k22 (env-ref $env_t41 $env k22)))))))
                    (make-env
                     $env_t41
                     (e18 (env-ref $env_t42 $env e18))
                     (i17 i17)
                     (k22 k22)))))
                (make-env $env_t42 (e18 e18)))
               1
               k21))
            (make-env $env_t40))
           (env-ref $env_t44 $env t14)
           (make-closure
            (lambda ($env rv26)
              (set-then! g$s rv26 (app* (env-ref $env_t43 $env k14) (void))))
            (make-env $env_t43 (k14 (env-ref $env_t44 $env k14)))))))
       (make-env $env_t44 (k14 k14) (t14 t14)))))
   (make-env $env_t40))
  (tuple "monkey" "spam")
  $halt))
