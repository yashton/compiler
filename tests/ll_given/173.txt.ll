(program
 (define-label
  $lambda47
  (lambda ($env rv17)
    (if rv17
      (app*
       (make-closure
        (lambda-label $lambda40)
        (make-env $env_t40 (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16))
      ((cps equal?)
       (env-ref $env_t44 $env n)
       0
       (make-closure
        (lambda-label $lambda46)
        (make-env
         $env_t44
         (k16 (env-ref $env_t44 $env k16))
         (n (env-ref $env_t44 $env n))
         (return (env-ref $env_t44 $env return))))))))
 (define-label
  $lambda48
  (lambda ($env k16)
    ((cps <)
     (env-ref $env_t43 $env n)
     0
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t44
       (k16 k16)
       (n (env-ref $env_t43 $env n))
       (return (env-ref $env_t43 $env return)))))))
 (define-label
  $lambda49
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda48)
      (make-env $env_t43 (n (env-ref $env_t45 $env n)) (return return)))
     k15)))
 (define-label
  $lambda50
  (lambda ($env n k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda49) (make-env $env_t45 (n n)))
     k14)))
 (define-label $lambda51 (lambda ($env rv25) ((cps py-print) rv25 $halt)))
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
  (lambda ($env k18) (app* (env-ref $env_t40 $env return) #f k18)))
 (define-label
  $lambda41
  (lambda ($env k20) (app* (env-ref $env_t40 $env return) 1 k20)))
 (define-label
  $lambda42
  (lambda ($env rv24)
    (app* (env-ref $env_t41 $env return) rv24 (env-ref $env_t41 $env k21))))
 (define-label
  $lambda43
  (lambda ($env rv23)
    ((cps *)
     (env-ref $env_t42 $env n)
     rv23
     (make-closure
      (lambda-label $lambda42)
      (make-env
       $env_t41
       (k21 (env-ref $env_t42 $env k21))
       (return (env-ref $env_t42 $env return)))))))
 (define-label
  $lambda44
  (lambda ($env rv22)
    (app*
     g$fact
     rv22
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t42
       (k21 (env-ref $env_t42 $env k21))
       (n (env-ref $env_t42 $env n))
       (return (env-ref $env_t42 $env return)))))))
 (define-label
  $lambda45
  (lambda ($env k21)
    ((cps -)
     (env-ref $env_t43 $env n)
     1
     (make-closure
      (lambda-label $lambda44)
      (make-env
       $env_t42
       (k21 k21)
       (n (env-ref $env_t43 $env n))
       (return (env-ref $env_t43 $env return)))))))
 (define-label
  $lambda46
  (lambda ($env rv19)
    (if rv19
      (app*
       (make-closure
        (lambda-label $lambda41)
        (make-env $env_t40 (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16))
      (app*
       (make-closure
        (lambda-label $lambda45)
        (make-env
         $env_t43
         (n (env-ref $env_t44 $env n))
         (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16)))))
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
  (make-closure (lambda-label $lambda50) (make-env $env_t39))
  (app* g$fact 5 (make-closure (lambda-label $lambda51) (make-env $env_t39)))))
