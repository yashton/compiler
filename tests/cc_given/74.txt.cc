(program
 (define-env $env_t43 (return))
 (define-env $env_t44 (k16 return))
 (define-env $env_t45 (a b return))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k17))
 (define-env $env_t41 (a b k17))
 (define-env $env_t42 (a b))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a_bigger (void))
 (set-then!
  g$a_bigger
  (make-closure
   (lambda ($env a b k14)
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
               (lambda ($env k17)
                 ((cps >)
                  (env-ref $env_t42 $env a)
                  (env-ref $env_t42 $env b)
                  (make-closure
                   (lambda ($env rv21)
                     (if rv21
                       ((cps -)
                        (env-ref $env_t41 $env a)
                        (env-ref $env_t41 $env b)
                        (make-closure
                         (lambda ($env rv22)
                           ((cps >=) rv22 2 (env-ref $env_t40 $env k17)))
                         (make-env
                          $env_t40
                          (k17 (env-ref $env_t41 $env k17)))))
                       (app* (env-ref $env_t41 $env k17) #f)))
                   (make-env
                    $env_t41
                    (a (env-ref $env_t42 $env a))
                    (b (env-ref $env_t42 $env b))
                    (k17 k17)))))
               (make-env
                $env_t42
                (a (env-ref $env_t45 $env a))
                (b (env-ref $env_t45 $env b))))
              (make-closure
               (lambda ($env rv18)
                 (if rv18
                   (app*
                    (make-closure
                     (lambda ($env k19)
                       (app* (env-ref $env_t43 $env return) #t k19))
                     (make-env
                      $env_t43
                      (return (env-ref $env_t44 $env return))))
                    (env-ref $env_t44 $env k16))
                   (app*
                    (make-closure
                     (lambda ($env k20)
                       (app* (env-ref $env_t43 $env return) #f k20))
                     (make-env
                      $env_t43
                      (return (env-ref $env_t44 $env return))))
                    (env-ref $env_t44 $env k16))))
               (make-env
                $env_t44
                (k16 k16)
                (return (env-ref $env_t45 $env return))))))
           (make-env
            $env_t45
            (a (env-ref $env_t42 $env a))
            (b (env-ref $env_t42 $env b))
            (return return)))
          k15))
       (make-env $env_t42 (a a) (b b)))
      k14))
   (make-env $env_t39))
  (app* $halt (void))))
