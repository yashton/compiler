(program
 (define-label
  $lambda51
  (lambda ($env break k14)
    (app*
     (make-closure (lambda-label $lambda50) (make-env $env_t39))
     (void)
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
  (lambda ($env rv20)
    (set-then! g$x rv20 ((cps py-print) g$x (env-ref $env_t40 $env k19)))))
 (define-label
  $lambda43
  (lambda ($env k19)
    ((cps -)
     g$x
     1
     (make-closure (lambda-label $lambda42) (make-env $env_t40 (k19 k19))))))
 (define-label
  $lambda44
  (lambda ($env continue k18)
    (app* (make-closure (lambda-label $lambda43) (make-env $env_t39)) k18)))
 (define-label
  $lambda45
  (lambda ($env rv21)
    (app*
     (get-cell (env-ref $env_t41 $env loop))
     (env-ref $env_t41 $env k16))))
 (define-label
  $lambda46
  (lambda ($env rv17)
    (if rv17
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure (lambda-label $lambda44) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda45)
        (make-env
         $env_t41
         (k16 (env-ref $env_t41 $env k16))
         (loop (env-ref $env_t41 $env loop)))))
      (app* (env-ref $env_t41 $env k16) (void)))))
 (define-label
  $lambda47
  (lambda ($env k16)
    ((cps >)
     g$x
     0
     (make-closure
      (lambda-label $lambda46)
      (make-env $env_t41 (k16 k16) (loop (env-ref $env_t42 $env loop)))))))
 (define-label
  $lambda48
  (lambda ($env k23) ((cps py-print) "didn't run\n" k23)))
 (define-label
  $lambda49
  (lambda ($env rv22)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     (env-ref $env_t43 $env k15))))
 (define-label
  $lambda50
  (lambda ($env loop k15)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure (lambda-label $lambda47) (make-env $env_t42 (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda49)
        (make-env $env_t43 (k15 k15))))))))
 (define-env $env_t42 (loop))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k19))
 (define-env $env_t43 (k15))
 (define-env $env_t41 (k16 loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  (app*
   (make-closure (lambda-label $lambda39) (make-env $env_t39))
   (make-closure (lambda-label $lambda51) (make-env $env_t39))
   $halt)))
