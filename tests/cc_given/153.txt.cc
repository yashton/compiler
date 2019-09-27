(program
 (define-env $env_t45 (k28))
 (define-env $env_t40 ($loop15 $seq14 k16))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t39 ())
 (define-env $env_t42 (k15))
 (define-env $env_t43 (e16 i15 k30))
 (define-env $env_t44 (e16))
 (define-env $env_t38 (cc))
 (define-env $env_t46 (k24))
 (define-env $env_t47 (i14))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fred (void))
 (define g$test (void))
 (define g$t (void))
 (set-then!
  g$fred
  (dict ("mike" 456) ("bill" 399) ("sarah" 521))
  (set-then!
   g$test
   (py-list* "zingo" "sarah" "bill" "wilma")
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
        g$test
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
                g$t
                (env-ref $env_t47 $env i14)
                (app*
                 (make-closure
                  (lambda ($env k24)
                    ((cps py-print)
                     g$t
                     (make-closure
                      (lambda ($env rv25)
                        (app*
                         get-field
                         g$fred
                         has_key
                         (make-closure
                          (lambda ($env rv26)
                            (app*
                             rv26
                             g$t
                             (make-closure
                              (lambda ($env rv27)
                                (if rv27
                                  (app*
                                   (make-closure
                                    (lambda ($env k28)
                                      (app*
                                       (make-closure
                                        (lambda ($env e16 k29)
                                          (app*
                                           (make-closure
                                            (lambda ($env i15 k30)
                                              ((cps py-list?)
                                               (env-ref $env_t44 $env e16)
                                               (make-closure
                                                (lambda ($env rv31)
                                                  (if rv31
                                                    ((cps py-list-ref)
                                                     (env-ref
                                                      $env_t43
                                                      $env
                                                      e16)
                                                     (env-ref
                                                      $env_t43
                                                      $env
                                                      i15)
                                                     (env-ref
                                                      $env_t43
                                                      $env
                                                      k30))
                                                    ((cps tuple?)
                                                     (env-ref
                                                      $env_t43
                                                      $env
                                                      e16)
                                                     (make-closure
                                                      (lambda ($env rv32)
                                                        (if rv32
                                                          ((cps tuple-ref)
                                                           (env-ref
                                                            $env_t43
                                                            $env
                                                            e16)
                                                           (env-ref
                                                            $env_t43
                                                            $env
                                                            i15)
                                                           (env-ref
                                                            $env_t43
                                                            $env
                                                            k30))
                                                          ((cps dict?)
                                                           (env-ref
                                                            $env_t43
                                                            $env
                                                            e16)
                                                           (make-closure
                                                            (lambda ($env rv33)
                                                              (if rv33
                                                                ((cps dict-ref)
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  e16)
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  i15)
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  k30))
                                                                (error
                                                                 "cannot index object"
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  k30))))
                                                            (make-env
                                                             $env_t43
                                                             (e16
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               e16))
                                                             (i15
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               i15))
                                                             (k30
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               k30)))))))
                                                      (make-env
                                                       $env_t43
                                                       (e16
                                                        (env-ref
                                                         $env_t43
                                                         $env
                                                         e16))
                                                       (i15
                                                        (env-ref
                                                         $env_t43
                                                         $env
                                                         i15))
                                                       (k30
                                                        (env-ref
                                                         $env_t43
                                                         $env
                                                         k30)))))))
                                                (make-env
                                                 $env_t43
                                                 (e16
                                                  (env-ref $env_t44 $env e16))
                                                 (i15 i15)
                                                 (k30 k30)))))
                                            (make-env $env_t44 (e16 e16)))
                                           g$t
                                           k29))
                                        (make-env $env_t39))
                                       g$fred
                                       (make-closure
                                        (lambda ($env rv34)
                                          ((cps py-print)
                                           "=>"
                                           rv34
                                           (env-ref $env_t45 $env k28)))
                                        (make-env $env_t45 (k28 k28)))))
                                    (make-env $env_t39))
                                   (env-ref $env_t46 $env k24))
                                  (app*
                                   (make-closure
                                    (lambda ($env k35)
                                      ((cps py-print) "is not present." k35))
                                    (make-env $env_t39))
                                   (env-ref $env_t46 $env k24))))
                              (make-env
                               $env_t46
                               (k24 (env-ref $env_t46 $env k24))))))
                          (make-env
                           $env_t46
                           (k24 (env-ref $env_t46 $env k24))))))
                      (make-env $env_t46 (k24 k24)))))
                  (make-env $env_t39))
                 k23)))
             (make-env $env_t47 (i14 i14)))
            k22))
         (make-env $env_t39))
        k14))
     (make-env $env_t39))
    $halt))))
