(program
 (define-label
  $lambda38
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda39
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda38) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda40
  (lambda ($env rv22) ((cps >=) rv22 2 (env-ref $env_t40 $env k17))))
 (define-label
  $lambda41
  (lambda ($env rv21)
    (if rv21
      ((cps -)
       (env-ref $env_t41 $env a)
       (env-ref $env_t41 $env b)
       (make-closure
        (lambda-label $lambda40)
        (make-env $env_t40 (k17 (env-ref $env_t41 $env k17)))))
      (app* (env-ref $env_t41 $env k17) #f))))
 (define-label
  $lambda42
  (lambda ($env k17)
    ((cps >)
     (env-ref $env_t42 $env a)
     (env-ref $env_t42 $env b)
     (make-closure
      (lambda-label $lambda41)
      (make-env
       $env_t41
       (a (env-ref $env_t42 $env a))
       (b (env-ref $env_t42 $env b))
       (k17 k17))))))
 (define-label
  $lambda43
  (lambda ($env k19) (app* (env-ref $env_t43 $env return) #t k19)))
 (define-label
  $lambda44
  (lambda ($env k20) (app* (env-ref $env_t43 $env return) #f k20)))
 (define-label
  $lambda45
  (lambda ($env rv18)
    (if rv18
      (app*
       (make-closure
        (lambda-label $lambda43)
        (make-env $env_t43 (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16))
      (app*
       (make-closure
        (lambda-label $lambda44)
        (make-env $env_t43 (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16)))))
 (define-label
  $lambda46
  (lambda ($env k16)
    (app*
     (make-closure
      (lambda-label $lambda42)
      (make-env
       $env_t42
       (a (env-ref $env_t45 $env a))
       (b (env-ref $env_t45 $env b))))
     (make-closure
      (lambda-label $lambda45)
      (make-env $env_t44 (k16 k16) (return (env-ref $env_t45 $env return)))))))
 (define-label
  $lambda47
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t45
       (a (env-ref $env_t42 $env a))
       (b (env-ref $env_t42 $env b))
       (return return)))
     k15)))
 (define-label
  $lambda48
  (lambda ($env a b k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda47) (make-env $env_t42 (a a) (b b)))
     k14)))
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
  (make-closure (lambda-label $lambda48) (make-env $env_t39))
  (app* $halt (void))))
