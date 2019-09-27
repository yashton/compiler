(program
 (define-env $env_t45 (k16 n return))
 (define-env $env_t38 (cc))
 (define-env $env_t42 (k21 return))
 (define-env $env_t43 (k21 return rv23))
 (define-env $env_t44 (k21 n return))
 (define-env $env_t39 ())
 (define-env $env_t40 (n))
 (define-env $env_t41 (n return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$Fibonacci (void))
 (set-then!
  g$Fibonacci
  (make-closure
   (lambda ($env n k14)
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
             ((cps equal?)
              (env-ref $env_t41 $env n)
              0
              (make-closure
               (lambda ($env rv18)
                 (app*
                  (make-closure
                   (lambda ($env t14 k17)
                     (if t14
                       (app* k17 t14)
                       ((cps equal?) (env-ref $env_t40 $env n) 1 k17)))
                   (make-env $env_t40 (n (env-ref $env_t45 $env n))))
                  rv18
                  (make-closure
                   (lambda ($env rv19)
                     (if rv19
                       (app*
                        (make-closure
                         (lambda ($env k20)
                           (app*
                            (env-ref $env_t41 $env return)
                            (env-ref $env_t41 $env n)
                            k20))
                         (make-env
                          $env_t41
                          (n (env-ref $env_t45 $env n))
                          (return (env-ref $env_t45 $env return))))
                        (env-ref $env_t45 $env k16))
                       (app*
                        (make-closure
                         (lambda ($env k21)
                           ((cps -)
                            (env-ref $env_t41 $env n)
                            1
                            (make-closure
                             (lambda ($env rv22)
                               (app*
                                g$Fibonacci
                                rv22
                                (make-closure
                                 (lambda ($env rv23)
                                   ((cps -)
                                    (env-ref $env_t44 $env n)
                                    2
                                    (make-closure
                                     (lambda ($env rv24)
                                       (app*
                                        g$Fibonacci
                                        rv24
                                        (make-closure
                                         (lambda ($env rv25)
                                           ((cps +)
                                            (env-ref $env_t43 $env rv23)
                                            rv25
                                            (make-closure
                                             (lambda ($env rv26)
                                               (app*
                                                (env-ref $env_t42 $env return)
                                                rv26
                                                (env-ref $env_t42 $env k21)))
                                             (make-env
                                              $env_t42
                                              (k21 (env-ref $env_t43 $env k21))
                                              (return
                                               (env-ref
                                                $env_t43
                                                $env
                                                return))))))
                                         (make-env
                                          $env_t43
                                          (k21 (env-ref $env_t43 $env k21))
                                          (return
                                           (env-ref $env_t43 $env return))
                                          (rv23
                                           (env-ref $env_t43 $env rv23))))))
                                     (make-env
                                      $env_t43
                                      (k21 (env-ref $env_t44 $env k21))
                                      (return (env-ref $env_t44 $env return))
                                      (rv23 rv23)))))
                                 (make-env
                                  $env_t44
                                  (k21 (env-ref $env_t44 $env k21))
                                  (n (env-ref $env_t44 $env n))
                                  (return (env-ref $env_t44 $env return))))))
                             (make-env
                              $env_t44
                              (k21 k21)
                              (n (env-ref $env_t41 $env n))
                              (return (env-ref $env_t41 $env return))))))
                         (make-env
                          $env_t41
                          (n (env-ref $env_t45 $env n))
                          (return (env-ref $env_t45 $env return))))
                        (env-ref $env_t45 $env k16))))
                   (make-env
                    $env_t45
                    (k16 (env-ref $env_t45 $env k16))
                    (n (env-ref $env_t45 $env n))
                    (return (env-ref $env_t45 $env return))))))
               (make-env
                $env_t45
                (k16 k16)
                (n (env-ref $env_t41 $env n))
                (return (env-ref $env_t41 $env return))))))
           (make-env $env_t41 (n (env-ref $env_t40 $env n)) (return return)))
          k15))
       (make-env $env_t40 (n n)))
      k14))
   (make-env $env_t39))
  (app* $halt (void))))
