(program
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t44 (rv23))
 (define-env $env_t45 (rv21))
 (define-env $env_t40 (k18 return))
 (define-env $env_t41 (return x))
 (define-env $env_t42 (k16 return x))
 (define-env $env_t43 (x))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$abs (void))
 (set-then!
  g$abs
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
             ((cps <)
              (env-ref $env_t41 $env x)
              0
              (make-closure
               (lambda ($env rv17)
                 (if rv17
                   (app*
                    (make-closure
                     (lambda ($env k18)
                       ((cps -)
                        (env-ref $env_t41 $env x)
                        (make-closure
                         (lambda ($env rv19)
                           (app*
                            (env-ref $env_t40 $env return)
                            rv19
                            (env-ref $env_t40 $env k18)))
                         (make-env
                          $env_t40
                          (k18 k18)
                          (return (env-ref $env_t41 $env return))))))
                     (make-env
                      $env_t41
                      (return (env-ref $env_t42 $env return))
                      (x (env-ref $env_t42 $env x))))
                    (env-ref $env_t42 $env k16))
                   (app*
                    (make-closure
                     (lambda ($env k20)
                       (app*
                        (env-ref $env_t41 $env return)
                        (env-ref $env_t41 $env x)
                        k20))
                     (make-env
                      $env_t41
                      (return (env-ref $env_t42 $env return))
                      (x (env-ref $env_t42 $env x))))
                    (env-ref $env_t42 $env k16))))
               (make-env
                $env_t42
                (k16 k16)
                (return (env-ref $env_t41 $env return))
                (x (env-ref $env_t41 $env x))))))
           (make-env $env_t41 (return return) (x (env-ref $env_t43 $env x))))
          k15))
       (make-env $env_t43 (x x)))
      k14))
   (make-env $env_t39))
  ((cps -)
   1
   (make-closure
    (lambda ($env rv21)
      ((cps -)
       1
       (make-closure
        (lambda ($env rv22)
          ((cps *)
           (env-ref $env_t45 $env rv21)
           rv22
           (make-closure
            (lambda ($env rv23)
              ((cps -)
               1
               (make-closure
                (lambda ($env rv24)
                  ((cps *)
                   (env-ref $env_t44 $env rv23)
                   rv24
                   (make-closure
                    (lambda ($env rv25)
                      (app*
                       g$abs
                       rv25
                       (make-closure
                        (lambda ($env rv26) ((cps py-print) rv26 $halt))
                        (make-env $env_t39))))
                    (make-env $env_t39))))
                (make-env $env_t44 (rv23 rv23)))))
            (make-env $env_t39))))
        (make-env $env_t45 (rv21 rv21)))))
    (make-env $env_t39)))))
