(program
 (define-label $lambda43 (lambda ($env rv19) ((cps py-print) g$a $halt)))
 (define-label
  $lambda38
  (lambda ($env rv18)
    (if rv18
      ((cps dict-set!)
       (env-ref $env_t38 $env b15)
       (env-ref $env_t38 $env i14)
       30
       (env-ref $env_t38 $env k15))
      (app* (env-ref $env_t38 $env k15) (void)))))
 (define-label
  $lambda39
  (lambda ($env rv17)
    (if rv17
      ((cps py-list-set!)
       (env-ref $env_t38 $env b15)
       (env-ref $env_t38 $env i14)
       30
       (env-ref $env_t38 $env k15))
      ((cps dict?)
       (env-ref $env_t38 $env b15)
       (make-closure
        (lambda-label $lambda38)
        (make-env
         $env_t38
         (b15 (env-ref $env_t38 $env b15))
         (i14 (env-ref $env_t38 $env i14))
         (k15 (env-ref $env_t38 $env k15))))))))
 (define-label
  $lambda40
  (lambda ($env rv16)
    (if rv16
      ((cps tuple-set!)
       (env-ref $env_t38 $env b15)
       (env-ref $env_t38 $env i14)
       30
       (env-ref $env_t38 $env k15))
      ((cps py-list?)
       (env-ref $env_t38 $env b15)
       (make-closure
        (lambda-label $lambda39)
        (make-env
         $env_t38
         (b15 (env-ref $env_t38 $env b15))
         (i14 (env-ref $env_t38 $env i14))
         (k15 (env-ref $env_t38 $env k15))))))))
 (define-label
  $lambda41
  (lambda ($env i14 k15)
    ((cps tuple?)
     (env-ref $env_t39 $env b15)
     (make-closure
      (lambda-label $lambda40)
      (make-env
       $env_t38
       (b15 (env-ref $env_t39 $env b15))
       (i14 i14)
       (k15 k15))))))
 (define-label
  $lambda42
  (lambda ($env b15 k14)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39 (b15 b15)))
     1
     k14)))
 (define-env $env_t38 (b15 i14 k15))
 (define-env $env_t39 (b15))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 0 1 2 3)
  (app*
   (make-closure (lambda-label $lambda42) (make-env $env_t40))
   g$a
   (make-closure (lambda-label $lambda43) (make-env $env_t40)))))
