(program
 (define-env $env_t44 (k16 return x))
 (define-env $env_t41 (k22 return))
 (define-env $env_t42 (k22 return x))
 (define-env $env_t43 (return x))
 (define-env $env_t39 ())
 (define-env $env_t45 (x))
 (define-env $env_t38 (cc))
 (define-env $env_t40 (return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fact (void))
 (define g$s (void))
 (set-then!
  g$fact
  (make-closure
   (lambda ($env x k14)
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
             ((cps -)
              1
              (make-closure
               (lambda ($env rv17)
                 ((cps equal?)
                  (env-ref $env_t44 $env x)
                  rv17
                  (make-closure
                   (lambda ($env rv18)
                     (if rv18
                       (app*
                        (make-closure
                         (lambda ($env k19)
                           (app* (env-ref $env_t40 $env return) 0+1.0i k19))
                         (make-env
                          $env_t40
                          (return (env-ref $env_t44 $env return))))
                        (env-ref $env_t44 $env k16))
                       ((cps equal?)
                        (env-ref $env_t44 $env x)
                        0
                        (make-closure
                         (lambda ($env rv20)
                           (if rv20
                             (app*
                              (make-closure
                               (lambda ($env k21)
                                 (app* (env-ref $env_t40 $env return) 1 k21))
                               (make-env
                                $env_t40
                                (return (env-ref $env_t44 $env return))))
                              (env-ref $env_t44 $env k16))
                             (app*
                              (make-closure
                               (lambda ($env k22)
                                 ((cps -)
                                  (env-ref $env_t43 $env x)
                                  1
                                  (make-closure
                                   (lambda ($env rv23)
                                     (app*
                                      g$fact
                                      rv23
                                      (make-closure
                                       (lambda ($env rv24)
                                         ((cps *)
                                          (env-ref $env_t42 $env x)
                                          rv24
                                          (make-closure
                                           (lambda ($env rv25)
                                             (app*
                                              (env-ref $env_t41 $env return)
                                              rv25
                                              (env-ref $env_t41 $env k22)))
                                           (make-env
                                            $env_t41
                                            (k22 (env-ref $env_t42 $env k22))
                                            (return
                                             (env-ref
                                              $env_t42
                                              $env
                                              return))))))
                                       (make-env
                                        $env_t42
                                        (k22 (env-ref $env_t42 $env k22))
                                        (return (env-ref $env_t42 $env return))
                                        (x (env-ref $env_t42 $env x))))))
                                   (make-env
                                    $env_t42
                                    (k22 k22)
                                    (return (env-ref $env_t43 $env return))
                                    (x (env-ref $env_t43 $env x))))))
                               (make-env
                                $env_t43
                                (return (env-ref $env_t44 $env return))
                                (x (env-ref $env_t44 $env x))))
                              (env-ref $env_t44 $env k16))))
                         (make-env
                          $env_t44
                          (k16 (env-ref $env_t44 $env k16))
                          (return (env-ref $env_t44 $env return))
                          (x (env-ref $env_t44 $env x)))))))
                   (make-env
                    $env_t44
                    (k16 (env-ref $env_t44 $env k16))
                    (return (env-ref $env_t44 $env return))
                    (x (env-ref $env_t44 $env x))))))
               (make-env
                $env_t44
                (k16 k16)
                (return (env-ref $env_t43 $env return))
                (x (env-ref $env_t43 $env x))))))
           (make-env $env_t43 (return return) (x (env-ref $env_t45 $env x))))
          k15))
       (make-env $env_t45 (x x)))
      k14))
   (make-env $env_t39))
  (set-then!
   g$s
   "foo\\ \n'\""
   (app*
    g$fact
    20
    (make-closure
     (lambda ($env rv26) ((cps py-print) rv26 $halt))
     (make-env $env_t39))))))
