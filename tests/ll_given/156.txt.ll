(program
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
 (define-label $lambda42 (lambda ($env k18) ((cps py-print) "here" k18)))
 (define-label
  $lambda43
  (lambda ($env continue k17)
    (app* (make-closure (lambda-label $lambda42) (make-env $env_t39)) k17)))
 (define-label
  $lambda44
  (lambda ($env rv19)
    (app*
     (get-cell (env-ref $env_t40 $env loop))
     (env-ref $env_t40 $env k16))))
 (define-label
  $lambda45
  (lambda ($env k16)
    (if #f
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure (lambda-label $lambda43) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda44)
        (make-env $env_t40 (k16 k16) (loop (env-ref $env_t41 $env loop)))))
      (app* k16 (void)))))
 (define-label
  $lambda46
  (lambda ($env rv20) (app* (env-ref $env_t42 $env k15) (void))))
 (define-label
  $lambda47
  (lambda ($env loop k15)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure (lambda-label $lambda45) (make-env $env_t41 (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda46)
        (make-env $env_t42 (k15 k15))))))))
 (define-label
  $lambda48
  (lambda ($env break k14)
    (app*
     (make-closure (lambda-label $lambda47) (make-env $env_t39))
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
 (define-env $env_t42 (k15))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (k16 loop))
 (define-env $env_t41 (loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (app*
  (make-closure (lambda-label $lambda39) (make-env $env_t39))
  (make-closure (lambda-label $lambda48) (make-env $env_t39))
  $halt))
