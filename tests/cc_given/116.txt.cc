(program
 (define-env $env_t40 ())
 (define-env $env_t41 (b15 i14 k15))
 (define-env $env_t44 (e21 i20 k32))
 (define-env $env_t45 (e21))
 (define-env $env_t46 (b15))
 (define-env $env_t42 (e19 i18 k25))
 (define-env $env_t43 (e19))
 (define-env $env_t38 (e17 i16 k18))
 (define-env $env_t39 (e17))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 1 2 3)
  (set-then!
   g$b
   (py-list* 1 2 3)
   (app*
    (make-closure
     (lambda ($env b15 k14)
       (app*
        (make-closure
         (lambda ($env i14 k15)
           ((cps tuple?)
            (env-ref $env_t46 $env b15)
            (make-closure
             (lambda ($env rv16)
               (if rv16
                 (app*
                  (make-closure
                   (lambda ($env e17 k17)
                     (app*
                      (make-closure
                       (lambda ($env i16 k18)
                         ((cps py-list?)
                          (env-ref $env_t39 $env e17)
                          (make-closure
                           (lambda ($env rv19)
                             (if rv19
                               ((cps py-list-ref)
                                (env-ref $env_t38 $env e17)
                                (env-ref $env_t38 $env i16)
                                (env-ref $env_t38 $env k18))
                               ((cps tuple?)
                                (env-ref $env_t38 $env e17)
                                (make-closure
                                 (lambda ($env rv20)
                                   (if rv20
                                     ((cps tuple-ref)
                                      (env-ref $env_t38 $env e17)
                                      (env-ref $env_t38 $env i16)
                                      (env-ref $env_t38 $env k18))
                                     ((cps dict?)
                                      (env-ref $env_t38 $env e17)
                                      (make-closure
                                       (lambda ($env rv21)
                                         (if rv21
                                           ((cps dict-ref)
                                            (env-ref $env_t38 $env e17)
                                            (env-ref $env_t38 $env i16)
                                            (env-ref $env_t38 $env k18))
                                           (error
                                            "cannot index object"
                                            (env-ref $env_t38 $env k18))))
                                       (make-env
                                        $env_t38
                                        (e17 (env-ref $env_t38 $env e17))
                                        (i16 (env-ref $env_t38 $env i16))
                                        (k18 (env-ref $env_t38 $env k18)))))))
                                 (make-env
                                  $env_t38
                                  (e17 (env-ref $env_t38 $env e17))
                                  (i16 (env-ref $env_t38 $env i16))
                                  (k18 (env-ref $env_t38 $env k18)))))))
                           (make-env
                            $env_t38
                            (e17 (env-ref $env_t39 $env e17))
                            (i16 i16)
                            (k18 k18)))))
                       (make-env $env_t39 (e17 e17)))
                      4
                      k17))
                   (make-env $env_t40))
                  g$a
                  (make-closure
                   (lambda ($env rv22)
                     ((cps tuple-set!)
                      (env-ref $env_t41 $env b15)
                      (env-ref $env_t41 $env i14)
                      rv22
                      (env-ref $env_t41 $env k15)))
                   (make-env
                    $env_t41
                    (b15 (env-ref $env_t41 $env b15))
                    (i14 (env-ref $env_t41 $env i14))
                    (k15 (env-ref $env_t41 $env k15)))))
                 ((cps py-list?)
                  (env-ref $env_t41 $env b15)
                  (make-closure
                   (lambda ($env rv23)
                     (if rv23
                       (app*
                        (make-closure
                         (lambda ($env e19 k24)
                           (app*
                            (make-closure
                             (lambda ($env i18 k25)
                               ((cps py-list?)
                                (env-ref $env_t43 $env e19)
                                (make-closure
                                 (lambda ($env rv26)
                                   (if rv26
                                     ((cps py-list-ref)
                                      (env-ref $env_t42 $env e19)
                                      (env-ref $env_t42 $env i18)
                                      (env-ref $env_t42 $env k25))
                                     ((cps tuple?)
                                      (env-ref $env_t42 $env e19)
                                      (make-closure
                                       (lambda ($env rv27)
                                         (if rv27
                                           ((cps tuple-ref)
                                            (env-ref $env_t42 $env e19)
                                            (env-ref $env_t42 $env i18)
                                            (env-ref $env_t42 $env k25))
                                           ((cps dict?)
                                            (env-ref $env_t42 $env e19)
                                            (make-closure
                                             (lambda ($env rv28)
                                               (if rv28
                                                 ((cps dict-ref)
                                                  (env-ref $env_t42 $env e19)
                                                  (env-ref $env_t42 $env i18)
                                                  (env-ref $env_t42 $env k25))
                                                 (error
                                                  "cannot index object"
                                                  (env-ref
                                                   $env_t42
                                                   $env
                                                   k25))))
                                             (make-env
                                              $env_t42
                                              (e19 (env-ref $env_t42 $env e19))
                                              (i18 (env-ref $env_t42 $env i18))
                                              (k25
                                               (env-ref
                                                $env_t42
                                                $env
                                                k25)))))))
                                       (make-env
                                        $env_t42
                                        (e19 (env-ref $env_t42 $env e19))
                                        (i18 (env-ref $env_t42 $env i18))
                                        (k25 (env-ref $env_t42 $env k25)))))))
                                 (make-env
                                  $env_t42
                                  (e19 (env-ref $env_t43 $env e19))
                                  (i18 i18)
                                  (k25 k25)))))
                             (make-env $env_t43 (e19 e19)))
                            4
                            k24))
                         (make-env $env_t40))
                        g$a
                        (make-closure
                         (lambda ($env rv29)
                           ((cps py-list-set!)
                            (env-ref $env_t41 $env b15)
                            (env-ref $env_t41 $env i14)
                            rv29
                            (env-ref $env_t41 $env k15)))
                         (make-env
                          $env_t41
                          (b15 (env-ref $env_t41 $env b15))
                          (i14 (env-ref $env_t41 $env i14))
                          (k15 (env-ref $env_t41 $env k15)))))
                       ((cps dict?)
                        (env-ref $env_t41 $env b15)
                        (make-closure
                         (lambda ($env rv30)
                           (if rv30
                             (app*
                              (make-closure
                               (lambda ($env e21 k31)
                                 (app*
                                  (make-closure
                                   (lambda ($env i20 k32)
                                     ((cps py-list?)
                                      (env-ref $env_t45 $env e21)
                                      (make-closure
                                       (lambda ($env rv33)
                                         (if rv33
                                           ((cps py-list-ref)
                                            (env-ref $env_t44 $env e21)
                                            (env-ref $env_t44 $env i20)
                                            (env-ref $env_t44 $env k32))
                                           ((cps tuple?)
                                            (env-ref $env_t44 $env e21)
                                            (make-closure
                                             (lambda ($env rv34)
                                               (if rv34
                                                 ((cps tuple-ref)
                                                  (env-ref $env_t44 $env e21)
                                                  (env-ref $env_t44 $env i20)
                                                  (env-ref $env_t44 $env k32))
                                                 ((cps dict?)
                                                  (env-ref $env_t44 $env e21)
                                                  (make-closure
                                                   (lambda ($env rv35)
                                                     (if rv35
                                                       ((cps dict-ref)
                                                        (env-ref
                                                         $env_t44
                                                         $env
                                                         e21)
                                                        (env-ref
                                                         $env_t44
                                                         $env
                                                         i20)
                                                        (env-ref
                                                         $env_t44
                                                         $env
                                                         k32))
                                                       (error
                                                        "cannot index object"
                                                        (env-ref
                                                         $env_t44
                                                         $env
                                                         k32))))
                                                   (make-env
                                                    $env_t44
                                                    (e21
                                                     (env-ref
                                                      $env_t44
                                                      $env
                                                      e21))
                                                    (i20
                                                     (env-ref
                                                      $env_t44
                                                      $env
                                                      i20))
                                                    (k32
                                                     (env-ref
                                                      $env_t44
                                                      $env
                                                      k32)))))))
                                             (make-env
                                              $env_t44
                                              (e21 (env-ref $env_t44 $env e21))
                                              (i20 (env-ref $env_t44 $env i20))
                                              (k32
                                               (env-ref
                                                $env_t44
                                                $env
                                                k32)))))))
                                       (make-env
                                        $env_t44
                                        (e21 (env-ref $env_t45 $env e21))
                                        (i20 i20)
                                        (k32 k32)))))
                                   (make-env $env_t45 (e21 e21)))
                                  4
                                  k31))
                               (make-env $env_t40))
                              g$a
                              (make-closure
                               (lambda ($env rv36)
                                 ((cps dict-set!)
                                  (env-ref $env_t41 $env b15)
                                  (env-ref $env_t41 $env i14)
                                  rv36
                                  (env-ref $env_t41 $env k15)))
                               (make-env
                                $env_t41
                                (b15 (env-ref $env_t41 $env b15))
                                (i14 (env-ref $env_t41 $env i14))
                                (k15 (env-ref $env_t41 $env k15)))))
                             (app* (env-ref $env_t41 $env k15) (void))))
                         (make-env
                          $env_t41
                          (b15 (env-ref $env_t41 $env b15))
                          (i14 (env-ref $env_t41 $env i14))
                          (k15 (env-ref $env_t41 $env k15)))))))
                   (make-env
                    $env_t41
                    (b15 (env-ref $env_t41 $env b15))
                    (i14 (env-ref $env_t41 $env i14))
                    (k15 (env-ref $env_t41 $env k15)))))))
             (make-env
              $env_t41
              (b15 (env-ref $env_t46 $env b15))
              (i14 i14)
              (k15 k15)))))
         (make-env $env_t46 (b15 b15)))
        1
        k14))
     (make-env $env_t40))
    g$a
    $halt))))
