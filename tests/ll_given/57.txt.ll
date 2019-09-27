(program
 (define-label
  $lambda47
  (lambda ($env rv18)
    (if rv18
      (app*
       (make-closure
        (lambda-label $lambda40)
        (make-env $env_t40 (return (env-ref $env_t44 $env return))))
       (env-ref $env_t44 $env k16))
      ((cps equal?)
       (env-ref $env_t44 $env x)
       0
       (make-closure
        (lambda-label $lambda46)
        (make-env
         $env_t44
         (k16 (env-ref $env_t44 $env k16))
         (return (env-ref $env_t44 $env return))
         (x (env-ref $env_t44 $env x))))))))
 (define-label
  $lambda48
  (lambda ($env rv17)
    ((cps equal?)
     (env-ref $env_t44 $env x)
     rv17
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t44
       (k16 (env-ref $env_t44 $env k16))
       (return (env-ref $env_t44 $env return))
       (x (env-ref $env_t44 $env x)))))))
 (define-label
  $lambda49
  (lambda ($env k16)
    ((cps -)
     1
     (make-closure
      (lambda-label $lambda48)
      (make-env
       $env_t44
       (k16 k16)
       (return (env-ref $env_t43 $env return))
       (x (env-ref $env_t43 $env x)))))))
 (define-label
  $lambda50
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda49)
      (make-env $env_t43 (return return) (x (env-ref $env_t45 $env x))))
     k15)))
 (define-label
  $lambda51
  (lambda ($env x k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda50) (make-env $env_t45 (x x)))
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
  (lambda ($env k19) (app* (env-ref $env_t40 $env return) 0+1.0i k19)))
 (define-label
  $lambda41
  (lambda ($env k21) (app* (env-ref $env_t40 $env return) 1 k21)))
 (define-label
  $lambda42
  (lambda ($env rv25)
    (app* (env-ref $env_t41 $env return) rv25 (env-ref $env_t41 $env k22))))
 (define-label
  $lambda43
  (lambda ($env rv24)
    ((cps *)
     (env-ref $env_t42 $env x)
     rv24
     (make-closure
      (lambda-label $lambda42)
      (make-env
       $env_t41
       (k22 (env-ref $env_t42 $env k22))
       (return (env-ref $env_t42 $env return)))))))
 (define-label
  $lambda44
  (lambda ($env rv23)
    (app*
     g$fact
     rv23
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t42
       (k22 (env-ref $env_t42 $env k22))
       (return (env-ref $env_t42 $env return))
       (x (env-ref $env_t42 $env x)))))))
 (define-label
  $lambda45
  (lambda ($env k22)
    ((cps -)
     (env-ref $env_t43 $env x)
     1
     (make-closure
      (lambda-label $lambda44)
      (make-env
       $env_t42
       (k22 k22)
       (return (env-ref $env_t43 $env return))
       (x (env-ref $env_t43 $env x)))))))
 (define-label
  $lambda46
  (lambda ($env rv20)
    (if rv20
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
         (return (env-ref $env_t44 $env return))
         (x (env-ref $env_t44 $env x))))
       (env-ref $env_t44 $env k16)))))
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
  (make-closure (lambda-label $lambda51) (make-env $env_t39))
  (set-then! g$s "foo\\ \n'\"" (app* g$fact 20 $halt))))
