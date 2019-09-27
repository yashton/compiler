(program
 (define-env $env_t44 (k16 n return))
 (define-env $env_t45 (n))
 (define-env $env_t41 (k21 return))
 (define-env $env_t42 (k21 n return))
 (define-env $env_t43 (n return))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fact (void))
 (set-then!
  g$fact
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
             ((cps <)
              (env-ref $env_t43 $env n)
              0
              (make-closure
               (lambda ($env rv17)
                 (if rv17
                   (app*
                    (make-closure
                     (lambda ($env k18)
                       (app* (env-ref $env_t40 $env return) #f k18))
                     (make-env
                      $env_t40
                      (return (env-ref $env_t44 $env return))))
                    (env-ref $env_t44 $env k16))
                   ((cps equal?)
                    (env-ref $env_t44 $env n)
                    0
                    (make-closure
                     (lambda ($env rv19)
                       (if rv19
                         (app*
                          (make-closure
                           (lambda ($env k20)
                             (app* (env-ref $env_t40 $env return) 1 k20))
                           (make-env
                            $env_t40
                            (return (env-ref $env_t44 $env return))))
                          (env-ref $env_t44 $env k16))
                         (app*
                          (make-closure
                           (lambda ($env k21)
                             ((cps -)
                              (env-ref $env_t43 $env n)
                              1
                              (make-closure
                               (lambda ($env rv22)
                                 (app*
                                  g$fact
                                  rv22
                                  (make-closure
                                   (lambda ($env rv23)
                                     ((cps *)
                                      (env-ref $env_t42 $env n)
                                      rv23
                                      (make-closure
                                       (lambda ($env rv24)
                                         (app*
                                          (env-ref $env_t41 $env return)
                                          rv24
                                          (env-ref $env_t41 $env k21)))
                                       (make-env
                                        $env_t41
                                        (k21 (env-ref $env_t42 $env k21))
                                        (return
                                         (env-ref $env_t42 $env return))))))
                                   (make-env
                                    $env_t42
                                    (k21 (env-ref $env_t42 $env k21))
                                    (n (env-ref $env_t42 $env n))
                                    (return (env-ref $env_t42 $env return))))))
                               (make-env
                                $env_t42
                                (k21 k21)
                                (n (env-ref $env_t43 $env n))
                                (return (env-ref $env_t43 $env return))))))
                           (make-env
                            $env_t43
                            (n (env-ref $env_t44 $env n))
                            (return (env-ref $env_t44 $env return))))
                          (env-ref $env_t44 $env k16))))
                     (make-env
                      $env_t44
                      (k16 (env-ref $env_t44 $env k16))
                      (n (env-ref $env_t44 $env n))
                      (return (env-ref $env_t44 $env return)))))))
               (make-env
                $env_t44
                (k16 k16)
                (n (env-ref $env_t43 $env n))
                (return (env-ref $env_t43 $env return))))))
           (make-env $env_t43 (n (env-ref $env_t45 $env n)) (return return)))
          k15))
       (make-env $env_t45 (n n)))
      k14))
   (make-env $env_t39))
  (app*
   g$fact
   5
   (make-closure
    (lambda ($env rv25) ((cps py-print) rv25 $halt))
    (make-env $env_t39)))))
