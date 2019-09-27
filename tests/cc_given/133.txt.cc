(program
 (define-env $env_t42 ($old-handler))
 (define-env $env_t43 ($old-handler k21))
 (define-env $env_t44 ($old-break $old-handler))
 (define-env $env_t45 ($old-continue $old-handler))
 (define-env $env_t46 ($old-break $old-continue $old-handler))
 (define-env $env_t40 ($ec15 k22))
 (define-env $env_t41 ($ec15 $old-handler))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (app*
  (make-closure
   (lambda ($env $old-handler k14)
     (app*
      (make-closure
       (lambda ($env $old-return k15)
         (app*
          (make-closure
           (lambda ($env $old-continue k16)
             (app*
              (make-closure
               (lambda ($env $old-break k17)
                 (app*
                  (make-closure
                   (lambda ($env return k18)
                     (app*
                      (make-closure
                       (lambda ($env continue k19)
                         (app*
                          (make-closure
                           (lambda ($env break k20)
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
                               (lambda ($env $ec15 k21)
                                 (set-then!
                                  $current-handler
                                  (make-closure
                                   (lambda ($env $ex14 k22)
                                     (set-then!
                                      $current-handler
                                      (env-ref $env_t41 $env $old-handler)
                                      (app*
                                       (make-closure
                                        (lambda ($env ex k23)
                                          (app*
                                           (make-closure
                                            (lambda ($env k24)
                                              ((cps py-print) 10 k24))
                                            (make-env $env_t39))
                                           k23))
                                        (make-env $env_t39))
                                       $ex14
                                       (make-closure
                                        (lambda ($env rv25)
                                          (app*
                                           (env-ref $env_t40 $env $ec15)
                                           rv25
                                           (env-ref $env_t40 $env k22)))
                                        (make-env
                                         $env_t40
                                         ($ec15 (env-ref $env_t41 $env $ec15))
                                         (k22 k22))))))
                                   (make-env
                                    $env_t41
                                    ($ec15 $ec15)
                                    ($old-handler
                                     (env-ref $env_t42 $env $old-handler))))
                                  (app*
                                   (make-closure
                                    (lambda ($env k27) ((cps py-print) 3 k27))
                                    (make-env $env_t39))
                                   (make-closure
                                    (lambda ($env rv28)
                                      (app*
                                       (make-closure
                                        (lambda ($env rv k26)
                                          (set-then!
                                           $current-handler
                                           (env-ref $env_t42 $env $old-handler)
                                           (app* k26 rv)))
                                        (make-env
                                         $env_t42
                                         ($old-handler
                                          (env-ref
                                           $env_t43
                                           $env
                                           $old-handler))))
                                       rv28
                                       (env-ref $env_t43 $env k21)))
                                    (make-env
                                     $env_t43
                                     ($old-handler
                                      (env-ref $env_t42 $env $old-handler))
                                     (k21 k21))))))
                               (make-env
                                $env_t42
                                ($old-handler
                                 (env-ref $env_t42 $env $old-handler))))
                              k20))
                           (make-env
                            $env_t42
                            ($old-handler
                             (env-ref $env_t44 $env $old-handler))))
                          (make-closure
                           (lambda ($env k29)
                             (set-then!
                              $current-handler
                              (env-ref $env_t44 $env $old-handler)
                              (app* (env-ref $env_t44 $env $old-break) k29)))
                           (make-env
                            $env_t44
                            ($old-break (env-ref $env_t44 $env $old-break))
                            ($old-handler
                             (env-ref $env_t44 $env $old-handler))))
                          k19))
                       (make-env
                        $env_t44
                        ($old-break (env-ref $env_t46 $env $old-break))
                        ($old-handler (env-ref $env_t46 $env $old-handler))))
                      (make-closure
                       (lambda ($env k30)
                         (set-then!
                          $current-handler
                          (env-ref $env_t45 $env $old-handler)
                          (app* (env-ref $env_t45 $env $old-continue) k30)))
                       (make-env
                        $env_t45
                        ($old-continue (env-ref $env_t46 $env $old-continue))
                        ($old-handler (env-ref $env_t46 $env $old-handler))))
                      k18))
                   (make-env
                    $env_t46
                    ($old-break $old-break)
                    ($old-continue (env-ref $env_t45 $env $old-continue))
                    ($old-handler (env-ref $env_t45 $env $old-handler))))
                  (make-closure
                   (lambda ($env rv k31)
                     (set-then!
                      $current-handler
                      (env-ref $env_t42 $env $old-handler)
                      (app* return rv k31)))
                   (make-env
                    $env_t42
                    ($old-handler (env-ref $env_t45 $env $old-handler))))
                  k17))
               (make-env
                $env_t45
                ($old-continue $old-continue)
                ($old-handler (env-ref $env_t42 $env $old-handler))))
              break
              k16))
           (make-env
            $env_t42
            ($old-handler (env-ref $env_t42 $env $old-handler))))
          continue
          k15))
       (make-env $env_t42 ($old-handler $old-handler)))
      return
      k14))
   (make-env $env_t39))
  $current-handler
  $halt))
