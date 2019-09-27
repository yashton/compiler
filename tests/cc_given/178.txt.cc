(program
 (define-env $env_t38 (b15 i14 k15))
 (define-env $env_t39 (b15))
 (define-env $env_t40 ())
 (define-env $env_t41 (e17 i16 k21))
 (define-env $env_t42 (e17))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 0 1 2 3)
  (app*
   (make-closure
    (lambda ($env b15 k14)
      (app*
       (make-closure
        (lambda ($env i14 k15)
          ((cps tuple?)
           (env-ref $env_t39 $env b15)
           (make-closure
            (lambda ($env rv16)
              (if rv16
                ((cps tuple-set!)
                 (env-ref $env_t38 $env b15)
                 (env-ref $env_t38 $env i14)
                 30
                 (env-ref $env_t38 $env k15))
                ((cps py-list?)
                 (env-ref $env_t38 $env b15)
                 (make-closure
                  (lambda ($env rv17)
                    (if rv17
                      ((cps py-list-set!)
                       (env-ref $env_t38 $env b15)
                       (env-ref $env_t38 $env i14)
                       30
                       (env-ref $env_t38 $env k15))
                      ((cps dict?)
                       (env-ref $env_t38 $env b15)
                       (make-closure
                        (lambda ($env rv18)
                          (if rv18
                            ((cps dict-set!)
                             (env-ref $env_t38 $env b15)
                             (env-ref $env_t38 $env i14)
                             30
                             (env-ref $env_t38 $env k15))
                            (app* (env-ref $env_t38 $env k15) (void))))
                        (make-env
                         $env_t38
                         (b15 (env-ref $env_t38 $env b15))
                         (i14 (env-ref $env_t38 $env i14))
                         (k15 (env-ref $env_t38 $env k15)))))))
                  (make-env
                   $env_t38
                   (b15 (env-ref $env_t38 $env b15))
                   (i14 (env-ref $env_t38 $env i14))
                   (k15 (env-ref $env_t38 $env k15)))))))
            (make-env
             $env_t38
             (b15 (env-ref $env_t39 $env b15))
             (i14 i14)
             (k15 k15)))))
        (make-env $env_t39 (b15 b15)))
       2
       k14))
    (make-env $env_t40))
   g$a
   (make-closure
    (lambda ($env rv19)
      (app*
       (make-closure
        (lambda ($env e17 k20)
          (app*
           (make-closure
            (lambda ($env i16 k21)
              ((cps py-list?)
               (env-ref $env_t42 $env e17)
               (make-closure
                (lambda ($env rv22)
                  (if rv22
                    ((cps py-list-ref)
                     (env-ref $env_t41 $env e17)
                     (env-ref $env_t41 $env i16)
                     (env-ref $env_t41 $env k21))
                    ((cps tuple?)
                     (env-ref $env_t41 $env e17)
                     (make-closure
                      (lambda ($env rv23)
                        (if rv23
                          ((cps tuple-ref)
                           (env-ref $env_t41 $env e17)
                           (env-ref $env_t41 $env i16)
                           (env-ref $env_t41 $env k21))
                          ((cps dict?)
                           (env-ref $env_t41 $env e17)
                           (make-closure
                            (lambda ($env rv24)
                              (if rv24
                                ((cps dict-ref)
                                 (env-ref $env_t41 $env e17)
                                 (env-ref $env_t41 $env i16)
                                 (env-ref $env_t41 $env k21))
                                (error
                                 "cannot index object"
                                 (env-ref $env_t41 $env k21))))
                            (make-env
                             $env_t41
                             (e17 (env-ref $env_t41 $env e17))
                             (i16 (env-ref $env_t41 $env i16))
                             (k21 (env-ref $env_t41 $env k21)))))))
                      (make-env
                       $env_t41
                       (e17 (env-ref $env_t41 $env e17))
                       (i16 (env-ref $env_t41 $env i16))
                       (k21 (env-ref $env_t41 $env k21)))))))
                (make-env
                 $env_t41
                 (e17 (env-ref $env_t42 $env e17))
                 (i16 i16)
                 (k21 k21)))))
            (make-env $env_t42 (e17 e17)))
           2
           k20))
        (make-env $env_t40))
       g$a
       (make-closure
        (lambda ($env rv25) ((cps py-print) rv25 $halt))
        (make-env $env_t40))))
    (make-env $env_t40)))))
