(program
 (define-label
  $lambda45
  (lambda ($env rv23)
    ((cps -)
     (env-ref $env_t44 $env n)
     2
     (make-closure
      (lambda-label $lambda44)
      (make-env
       $env_t43
       (k21 (env-ref $env_t44 $env k21))
       (return (env-ref $env_t44 $env return))
       (rv23 rv23))))))
 (define-label
  $lambda46
  (lambda ($env rv22)
    (app*
     g$Fibonacci
     rv22
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t44
       (k21 (env-ref $env_t44 $env k21))
       (n (env-ref $env_t44 $env n))
       (return (env-ref $env_t44 $env return)))))))
 (define-label
  $lambda47
  (lambda ($env k21)
    ((cps -)
     (env-ref $env_t41 $env n)
     1
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t44
       (k21 k21)
       (n (env-ref $env_t41 $env n))
       (return (env-ref $env_t41 $env return)))))))
 (define-label
  $lambda48
  (lambda ($env rv19)
    (if rv19
      (app*
       (make-closure
        (lambda-label $lambda41)
        (make-env
         $env_t41
         (n (env-ref $env_t45 $env n))
         (return (env-ref $env_t45 $env return))))
       (env-ref $env_t45 $env k16))
      (app*
       (make-closure
        (lambda-label $lambda47)
        (make-env
         $env_t41
         (n (env-ref $env_t45 $env n))
         (return (env-ref $env_t45 $env return))))
       (env-ref $env_t45 $env k16)))))
 (define-label
  $lambda49
  (lambda ($env rv18)
    (app*
     (make-closure
      (lambda-label $lambda40)
      (make-env $env_t40 (n (env-ref $env_t45 $env n))))
     rv18
     (make-closure
      (lambda-label $lambda48)
      (make-env
       $env_t45
       (k16 (env-ref $env_t45 $env k16))
       (n (env-ref $env_t45 $env n))
       (return (env-ref $env_t45 $env return)))))))
 (define-label
  $lambda50
  (lambda ($env k16)
    ((cps equal?)
     (env-ref $env_t41 $env n)
     0
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t45
       (k16 k16)
       (n (env-ref $env_t41 $env n))
       (return (env-ref $env_t41 $env return)))))))
 (define-label
  $lambda51
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda50)
      (make-env $env_t41 (n (env-ref $env_t40 $env n)) (return return)))
     k15)))
 (define-label
  $lambda52
  (lambda ($env n k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda51) (make-env $env_t40 (n n)))
     k14)))
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
  (lambda ($env t14 k17)
    (if t14 (app* k17 t14) ((cps equal?) (env-ref $env_t40 $env n) 1 k17))))
 (define-label
  $lambda41
  (lambda ($env k20)
    (app* (env-ref $env_t41 $env return) (env-ref $env_t41 $env n) k20)))
 (define-label
  $lambda42
  (lambda ($env rv26)
    (app* (env-ref $env_t42 $env return) rv26 (env-ref $env_t42 $env k21))))
 (define-label
  $lambda43
  (lambda ($env rv25)
    ((cps +)
     (env-ref $env_t43 $env rv23)
     rv25
     (make-closure
      (lambda-label $lambda42)
      (make-env
       $env_t42
       (k21 (env-ref $env_t43 $env k21))
       (return (env-ref $env_t43 $env return)))))))
 (define-label
  $lambda44
  (lambda ($env rv24)
    (app*
     g$Fibonacci
     rv24
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t43
       (k21 (env-ref $env_t43 $env k21))
       (return (env-ref $env_t43 $env return))
       (rv23 (env-ref $env_t43 $env rv23)))))))
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
  (make-closure (lambda-label $lambda52) (make-env $env_t39))
  (app* $halt (void))))
