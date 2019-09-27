(program
 (define-label
  $lambda49
  (lambda ($env k25) ((cps py-print) "didn't run\n" k25)))
 (define-label
  $lambda50
  (lambda ($env rv24)
    (app*
     (make-closure (lambda-label $lambda49) (make-env $env_t39))
     (env-ref $env_t43 $env k16))))
 (define-label
  $lambda51
  (lambda ($env loop k16)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure (lambda-label $lambda48) (make-env $env_t42 (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda50)
        (make-env $env_t43 (k16 k16))))))))
 (define-label
  $lambda52
  (lambda ($env break k15)
    (app*
     (make-closure (lambda-label $lambda51) (make-env $env_t39))
     (void)
     k15)))
 (define-label
  $lambda53
  (lambda ($env rv14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda52) (make-env $env_t39))
     $halt)))
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
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda41
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda40) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda42
  (lambda ($env rv22)
    (set-then! g$x rv22 ((cps py-print) g$x (env-ref $env_t40 $env k20)))))
 (define-label
  $lambda43
  (lambda ($env rv21)
    ((cps -)
     g$x
     1
     (make-closure
      (lambda-label $lambda42)
      (make-env $env_t40 (k20 (env-ref $env_t40 $env k20)))))))
 (define-label
  $lambda44
  (lambda ($env k20)
    ((cps py-print)
     g$x
     (make-closure (lambda-label $lambda43) (make-env $env_t40 (k20 k20))))))
 (define-label
  $lambda45
  (lambda ($env continue k19)
    (app* (make-closure (lambda-label $lambda44) (make-env $env_t39)) k19)))
 (define-label
  $lambda46
  (lambda ($env rv23)
    (app*
     (get-cell (env-ref $env_t41 $env loop))
     (env-ref $env_t41 $env k17))))
 (define-label
  $lambda47
  (lambda ($env rv18)
    (if rv18
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure (lambda-label $lambda45) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda46)
        (make-env
         $env_t41
         (k17 (env-ref $env_t41 $env k17))
         (loop (env-ref $env_t41 $env loop)))))
      (app* (env-ref $env_t41 $env k17) (void)))))
 (define-label
  $lambda48
  (lambda ($env k17)
    ((cps >)
     g$x
     0
     (make-closure
      (lambda-label $lambda47)
      (make-env $env_t41 (k17 k17) (loop (env-ref $env_t42 $env loop)))))))
 (define-env $env_t43 (k16))
 (define-env $env_t41 (k17 loop))
 (define-env $env_t42 (loop))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k20))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  ((cps py-print)
   g$x
   (make-closure (lambda-label $lambda53) (make-env $env_t39)))))
