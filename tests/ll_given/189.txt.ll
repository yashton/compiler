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
    (set-cell!
     (env-ref $env_t40 $env called)
     rv20
     (app* (env-ref $env_t40 $env k19) (void)))))
 (define-label
  $lambda43
  (lambda ($env k19)
    ((cps +)
     (get-cell (env-ref $env_t41 $env called))
     1
     (make-closure
      (lambda-label $lambda42)
      (make-env $env_t40 (called (env-ref $env_t41 $env called)) (k19 k19))))))
 (define-label
  $lambda44
  (lambda ($env return k18)
    (app*
     (make-closure
      (lambda-label $lambda43)
      (make-env $env_t41 (called (env-ref $env_t41 $env called))))
     k18)))
 (define-label
  $lambda45
  (lambda ($env k17)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39))
     (make-closure
      (lambda-label $lambda44)
      (make-env $env_t41 (called (env-ref $env_t41 $env called))))
     k17)))
 (define-label
  $lambda46
  (lambda ($env rv22)
    ((cps py-print)
     (get-cell (env-ref $env_t42 $env called))
     (env-ref $env_t42 $env k16))))
 (define-label
  $lambda47
  (lambda ($env rv21)
    (app*
     (get-cell (env-ref $env_t43 $env func))
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t42
       (called (env-ref $env_t43 $env called))
       (k16 (env-ref $env_t43 $env k16)))))))
 (define-label
  $lambda48
  (lambda ($env called func k16)
    (set-then!
     called
     (make-cell called)
     (set-then!
      func
      (make-cell func)
      (set-cell!
       called
       0
       (set-cell!
        func
        (make-closure
         (lambda-label $lambda45)
         (make-env $env_t41 (called called)))
        (app*
         (get-cell func)
         (make-closure
          (lambda-label $lambda47)
          (make-env $env_t43 (called called) (func func) (k16 k16))))))))))
 (define-label
  $lambda49
  (lambda ($env return k15)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     (void)
     (void)
     k15)))
 (define-label
  $lambda50
  (lambda ($env k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda49) (make-env $env_t39))
     k14)))
 (define-label
  $lambda51
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda52
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda51) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda53
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda54
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda53) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda55
  (lambda ($env rv29)
    (set-then! g$called rv29 (app* (env-ref $env_t44 $env k28) (void)))))
 (define-label
  $lambda56
  (lambda ($env k28)
    ((cps +)
     g$called
     1
     (make-closure (lambda-label $lambda55) (make-env $env_t44 (k28 k28))))))
 (define-label
  $lambda57
  (lambda ($env return k27)
    (app* (make-closure (lambda-label $lambda56) (make-env $env_t39)) k27)))
 (define-label
  $lambda58
  (lambda ($env k26)
    (app*
     (make-closure (lambda-label $lambda54) (make-env $env_t39))
     (make-closure (lambda-label $lambda57) (make-env $env_t39))
     k26)))
 (define-label
  $lambda59
  (lambda ($env rv30)
    (app*
     (get-cell (env-ref $env_t45 $env func))
     (env-ref $env_t45 $env k25))))
 (define-label
  $lambda60
  (lambda ($env func k25)
    (set-then!
     func
     (make-cell func)
     (set-cell!
      func
      (make-closure (lambda-label $lambda58) (make-env $env_t39))
      (app*
       (get-cell func)
       (make-closure
        (lambda-label $lambda59)
        (make-env $env_t45 (func func) (k25 k25))))))))
 (define-label
  $lambda61
  (lambda ($env return k24)
    (app*
     (make-closure (lambda-label $lambda60) (make-env $env_t39))
     (void)
     k24)))
 (define-label
  $lambda62
  (lambda ($env k23)
    (app*
     (make-closure (lambda-label $lambda52) (make-env $env_t39))
     (make-closure (lambda-label $lambda61) (make-env $env_t39))
     k23)))
 (define-label $lambda63 (lambda ($env rv32) ((cps py-print) g$called $halt)))
 (define-label
  $lambda64
  (lambda ($env rv31)
    (app*
     g$outfunc2
     (make-closure (lambda-label $lambda63) (make-env $env_t39)))))
 (define-env $env_t40 (called k19))
 (define-env $env_t41 (called))
 (define-env $env_t44 (k28))
 (define-env $env_t45 (func k25))
 (define-env $env_t42 (called k16))
 (define-env $env_t43 (called func k16))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$outfunc2 (void))
 (define g$called (void))
 (define g$outfunc (void))
 (set-then!
  g$outfunc
  (make-closure (lambda-label $lambda50) (make-env $env_t39))
  (set-then!
   g$called
   0
   (set-then!
    g$outfunc2
    (make-closure (lambda-label $lambda62) (make-env $env_t39))
    (app*
     g$outfunc
     (make-closure (lambda-label $lambda64) (make-env $env_t39)))))))
