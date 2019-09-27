(program
 (define-label
  $lambda41
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda40) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda42
  (lambda ($env k19) (set-then! g$x 30 (app* k19 (void)))))
 (define-label
  $lambda43
  (lambda ($env return k18)
    (app* (make-closure (lambda-label $lambda42) (make-env $env_t39)) k18)))
 (define-label
  $lambda44
  (lambda ($env k17)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39))
     (make-closure (lambda-label $lambda43) (make-env $env_t39))
     k17)))
 (define-label
  $lambda45
  (lambda ($env x g k16)
    (set-then!
     x
     (make-cell x)
     (set-then!
      g
      (make-cell g)
      (set-cell!
       x
       20
       (set-cell!
        g
        (make-closure (lambda-label $lambda44) (make-env $env_t39))
        (app* (get-cell g) k16)))))))
 (define-label
  $lambda46
  (lambda ($env return k15)
    (app*
     (make-closure (lambda-label $lambda45) (make-env $env_t39))
     (void)
     (void)
     k15)))
 (define-label
  $lambda47
  (lambda ($env k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda46) (make-env $env_t39))
     k14)))
 (define-label $lambda48 (lambda ($env rv20) ((cps py-print) g$x $halt)))
 (define-label
  $lambda38
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-label
  $lambda39
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda38) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda40
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$x (void))
 (set-then!
  g$x
  10
  (set-then!
   g$f
   (make-closure (lambda-label $lambda47) (make-env $env_t39))
   (app* g$f (make-closure (lambda-label $lambda48) (make-env $env_t39))))))
