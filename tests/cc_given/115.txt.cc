(program
 (define-env $env_t38 (e15 i14 k15))
 (define-env $env_t39 (e15))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 1 2 3)
  (app*
   (make-closure
    (lambda ($env e15 k14)
      (app*
       (make-closure
        (lambda ($env i14 k15)
          ((cps py-list?)
           (env-ref $env_t39 $env e15)
           (make-closure
            (lambda ($env rv16)
              (if rv16
                ((cps py-list-ref)
                 (env-ref $env_t38 $env e15)
                 (env-ref $env_t38 $env i14)
                 (env-ref $env_t38 $env k15))
                ((cps tuple?)
                 (env-ref $env_t38 $env e15)
                 (make-closure
                  (lambda ($env rv17)
                    (if rv17
                      ((cps tuple-ref)
                       (env-ref $env_t38 $env e15)
                       (env-ref $env_t38 $env i14)
                       (env-ref $env_t38 $env k15))
                      ((cps dict?)
                       (env-ref $env_t38 $env e15)
                       (make-closure
                        (lambda ($env rv18)
                          (if rv18
                            ((cps dict-ref)
                             (env-ref $env_t38 $env e15)
                             (env-ref $env_t38 $env i14)
                             (env-ref $env_t38 $env k15))
                            (error
                             "cannot index object"
                             (env-ref $env_t38 $env k15))))
                        (make-env
                         $env_t38
                         (e15 (env-ref $env_t38 $env e15))
                         (i14 (env-ref $env_t38 $env i14))
                         (k15 (env-ref $env_t38 $env k15)))))))
                  (make-env
                   $env_t38
                   (e15 (env-ref $env_t38 $env e15))
                   (i14 (env-ref $env_t38 $env i14))
                   (k15 (env-ref $env_t38 $env k15)))))))
            (make-env
             $env_t38
             (e15 (env-ref $env_t39 $env e15))
             (i14 i14)
             (k15 k15)))))
        (make-env $env_t39 (e15 e15)))
       1
       k14))
    (make-env $env_t40))
   g$a
   (make-closure
    (lambda ($env rv19) ((cps py-print) rv19 $halt))
    (make-env $env_t40)))))
