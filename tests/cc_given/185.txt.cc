(program
 (define-env $env_t49 (return))
 (define-env $env_t46 ($old-break $old-continue $old-handler))
 (define-env $env_t42 ($old-handler))
 (define-env $env_t43 ($old-handler k24))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t45 ($old-continue $old-handler))
 (define-env $env_t44 ($old-break $old-handler))
 (define-env $env_t47 ($old-handler return))
 (define-env $env_t48 ($old-continue $old-handler return))
 (define-env $env_t40 ($ec15 k25))
 (define-env $env_t41 ($ec15 $old-handler))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$doomed (void))
 (set-then!
  g$doomed
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
           (lambda ($env k16)
             (app*
              (make-closure
               (lambda ($env $old-handler k17)
                 (app*
                  (make-closure
                   (lambda ($env $old-return k18)
                     (app*
                      (make-closure
                       (lambda ($env $old-continue k19)
                         (app*
                          (make-closure
                           (lambda ($env $old-break k20)
                             (app*
                              (make-closure
                               (lambda ($env return k21)
                                 (app*
                                  (make-closure
                                   (lambda ($env continue k22)
                                     (app*
                                      (make-closure
                                       (lambda ($env break k23)
                                         (app*
                                          (make-closure
                                           (lambda ($env f cc)
                                             (app*
                                              f
                                              (make-closure
                                               (lambda ($env x k)
                                                 (app*
                                                  (env-ref $env_t38 $env cc)
                                                  x))
                                               (make-env $env_t38 (cc cc)))
                                              cc))
                                           (make-env $env_t39))
                                          (make-closure
                                           (lambda ($env $ec15 k24)
                                             (set-then!
                                              $current-handler
                                              (make-closure
                                               (lambda ($env $ex14 k25)
                                                 (set-then!
                                                  $current-handler
                                                  (env-ref
                                                   $env_t41
                                                   $env
                                                   $old-handler)
                                                  (app*
                                                   (make-closure
                                                    (lambda ($env ex k26)
                                                      (app*
                                                       (make-closure
                                                        (lambda ($env k27)
                                                          ((cps py-print)
                                                           "caught an index error!"
                                                           k27))
                                                        (make-env $env_t39))
                                                       k26))
                                                    (make-env $env_t39))
                                                   $ex14
                                                   (make-closure
                                                    (lambda ($env rv28)
                                                      (app*
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        $ec15)
                                                       rv28
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        k25)))
                                                    (make-env
                                                     $env_t40
                                                     ($ec15
                                                      (env-ref
                                                       $env_t41
                                                       $env
                                                       $ec15))
                                                     (k25 k25))))))
                                               (make-env
                                                $env_t41
                                                ($ec15 $ec15)
                                                ($old-handler
                                                 (env-ref
                                                  $env_t42
                                                  $env
                                                  $old-handler))))
                                              (app*
                                               (make-closure
                                                (lambda ($env k30)
                                                  ((cps py-print)
                                                   "try...\n"
                                                   k30))
                                                (make-env $env_t39))
                                               (make-closure
                                                (lambda ($env rv31)
                                                  (app*
                                                   (make-closure
                                                    (lambda ($env rv k29)
                                                      (set-then!
                                                       $current-handler
                                                       (env-ref
                                                        $env_t42
                                                        $env
                                                        $old-handler)
                                                       (app* k29 rv)))
                                                    (make-env
                                                     $env_t42
                                                     ($old-handler
                                                      (env-ref
                                                       $env_t43
                                                       $env
                                                       $old-handler))))
                                                   rv31
                                                   (env-ref
                                                    $env_t43
                                                    $env
                                                    k24)))
                                                (make-env
                                                 $env_t43
                                                 ($old-handler
                                                  (env-ref
                                                   $env_t42
                                                   $env
                                                   $old-handler))
                                                 (k24 k24))))))
                                           (make-env
                                            $env_t42
                                            ($old-handler
                                             (env-ref
                                              $env_t42
                                              $env
                                              $old-handler))))
                                          k23))
                                       (make-env
                                        $env_t42
                                        ($old-handler
                                         (env-ref
                                          $env_t44
                                          $env
                                          $old-handler))))
                                      (make-closure
                                       (lambda ($env k32)
                                         (set-then!
                                          $current-handler
                                          (env-ref $env_t44 $env $old-handler)
                                          (app*
                                           (env-ref $env_t44 $env $old-break)
                                           k32)))
                                       (make-env
                                        $env_t44
                                        ($old-break
                                         (env-ref $env_t44 $env $old-break))
                                        ($old-handler
                                         (env-ref
                                          $env_t44
                                          $env
                                          $old-handler))))
                                      k22))
                                   (make-env
                                    $env_t44
                                    ($old-break
                                     (env-ref $env_t46 $env $old-break))
                                    ($old-handler
                                     (env-ref $env_t46 $env $old-handler))))
                                  (make-closure
                                   (lambda ($env k33)
                                     (set-then!
                                      $current-handler
                                      (env-ref $env_t45 $env $old-handler)
                                      (app*
                                       (env-ref $env_t45 $env $old-continue)
                                       k33)))
                                   (make-env
                                    $env_t45
                                    ($old-continue
                                     (env-ref $env_t46 $env $old-continue))
                                    ($old-handler
                                     (env-ref $env_t46 $env $old-handler))))
                                  k21))
                               (make-env
                                $env_t46
                                ($old-break $old-break)
                                ($old-continue
                                 (env-ref $env_t48 $env $old-continue))
                                ($old-handler
                                 (env-ref $env_t48 $env $old-handler))))
                              (make-closure
                               (lambda ($env rv k34)
                                 (set-then!
                                  $current-handler
                                  (env-ref $env_t47 $env $old-handler)
                                  (app*
                                   (env-ref $env_t47 $env return)
                                   rv
                                   k34)))
                               (make-env
                                $env_t47
                                ($old-handler
                                 (env-ref $env_t48 $env $old-handler))
                                (return (env-ref $env_t48 $env return))))
                              k20))
                           (make-env
                            $env_t48
                            ($old-continue $old-continue)
                            ($old-handler (env-ref $env_t47 $env $old-handler))
                            (return (env-ref $env_t47 $env return))))
                          break
                          k19))
                       (make-env
                        $env_t47
                        ($old-handler (env-ref $env_t47 $env $old-handler))
                        (return (env-ref $env_t47 $env return))))
                      continue
                      k18))
                   (make-env
                    $env_t47
                    ($old-handler $old-handler)
                    (return (env-ref $env_t49 $env return))))
                  (env-ref $env_t49 $env return)
                  k17))
               (make-env $env_t49 (return (env-ref $env_t49 $env return))))
              $current-handler
              k16))
           (make-env $env_t49 (return return)))
          k15))
       (make-env $env_t39))
      k14))
   (make-env $env_t39))
  (app* g$doomed $halt)))
