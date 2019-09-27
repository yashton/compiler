(program
 (define-env $env_t50 (k15))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (break))
 (define-env $env_t41 (break k20))
 (define-env $env_t42 (continue))
 (define-env $env_t43 (continue k22))
 (define-env $env_t44 (k18))
 (define-env $env_t45 (continue k18))
 (define-env $env_t46 (break continue k18))
 (define-env $env_t47 (break continue))
 (define-env $env_t48 (k16 loop))
 (define-env $env_t49 (break loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$count (void))
 (set-then!
  g$count
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
               (if #t
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
                   (lambda ($env continue k17)
                     (app*
                      (make-closure
                       (lambda ($env k18)
                         ((cps +)
                          g$count
                          1
                          (make-closure
                           (lambda ($env rv19)
                             (set-then!
                              g$count
                              rv19
                              (app*
                               (make-closure
                                (lambda ($env k20)
                                  ((cps >)
                                   g$count
                                   10
                                   (make-closure
                                    (lambda ($env rv26)
                                      (if rv26
                                        (app*
                                         (make-closure
                                          (lambda ($env k27)
                                            (app*
                                             (env-ref $env_t40 $env break)
                                             k27))
                                          (make-env
                                           $env_t40
                                           (break
                                            (env-ref $env_t41 $env break))))
                                         (env-ref $env_t41 $env k20))
                                        (app*
                                         (env-ref $env_t41 $env k20)
                                         (void))))
                                    (make-env
                                     $env_t41
                                     (break (env-ref $env_t40 $env break))
                                     (k20 k20)))))
                                (make-env
                                 $env_t40
                                 (break (env-ref $env_t46 $env break))))
                               (make-closure
                                (lambda ($env rv21)
                                  (app*
                                   (make-closure
                                    (lambda ($env k22)
                                      ((cps equal?)
                                       g$count
                                       5
                                       (make-closure
                                        (lambda ($env rv24)
                                          (if rv24
                                            (app*
                                             (make-closure
                                              (lambda ($env k25)
                                                (app*
                                                 (env-ref
                                                  $env_t42
                                                  $env
                                                  continue)
                                                 k25))
                                              (make-env
                                               $env_t42
                                               (continue
                                                (env-ref
                                                 $env_t43
                                                 $env
                                                 continue))))
                                             (env-ref $env_t43 $env k22))
                                            (app*
                                             (env-ref $env_t43 $env k22)
                                             (void))))
                                        (make-env
                                         $env_t43
                                         (continue
                                          (env-ref $env_t42 $env continue))
                                         (k22 k22)))))
                                    (make-env
                                     $env_t42
                                     (continue
                                      (env-ref $env_t45 $env continue))))
                                   (make-closure
                                    (lambda ($env rv23)
                                      ((cps py-print)
                                       g$count
                                       (env-ref $env_t44 $env k18)))
                                    (make-env
                                     $env_t44
                                     (k18 (env-ref $env_t45 $env k18))))))
                                (make-env
                                 $env_t45
                                 (continue (env-ref $env_t46 $env continue))
                                 (k18 (env-ref $env_t46 $env k18)))))))
                           (make-env
                            $env_t46
                            (break (env-ref $env_t47 $env break))
                            (continue (env-ref $env_t47 $env continue))
                            (k18 k18)))))
                       (make-env
                        $env_t47
                        (break (env-ref $env_t40 $env break))
                        (continue continue)))
                      k17))
                   (make-env $env_t40 (break (env-ref $env_t49 $env break))))
                  (make-closure
                   (lambda ($env rv28)
                     (app*
                      (get-cell (env-ref $env_t48 $env loop))
                      (env-ref $env_t48 $env k16)))
                   (make-env
                    $env_t48
                    (k16 k16)
                    (loop (env-ref $env_t49 $env loop)))))
                 (app* k16 (void))))
             (make-env
              $env_t49
              (break (env-ref $env_t40 $env break))
              (loop loop)))
            (app*
             (get-cell loop)
             (make-closure
              (lambda ($env rv29) (app* (env-ref $env_t50 $env k15) (void)))
              (make-env $env_t50 (k15 k15)))))))
        (make-env $env_t40 (break break)))
       (void)
       k14))
    (make-env $env_t39))
   $halt)))
