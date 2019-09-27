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
  (lambda ($env rv21)
    (if rv21
      (app*
       for-dict-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k16))
      (app* (env-ref $env_t40 $env k16) (void)))))
 (define-label
  $lambda41
  (lambda ($env rv20)
    (if rv20
      (app*
       for-py-list-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k16))
      ((cps dict?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda40)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k16 (env-ref $env_t40 $env k16))))))))
 (define-label
  $lambda42
  (lambda ($env rv19)
    (if rv19
      (app*
       for-tuple-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k16))
      ((cps py-list?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda41)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k16 (env-ref $env_t40 $env k16))))))))
 (define-label
  $lambda43
  (lambda ($env rv18)
    (if rv18
      (app*
       for-set-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k16))
      ((cps tuple?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda42)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k16 (env-ref $env_t40 $env k16))))))))
 (define-label
  $lambda44
  (lambda ($env k16)
    ((cps set?)
     (env-ref $env_t41 $env $seq14)
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t40
       ($loop15 (env-ref $env_t41 $env $loop15))
       ($seq14 (env-ref $env_t41 $env $seq14))
       (k16 k16))))))
 (define-label
  $lambda45
  (lambda ($env rv17) (app* (env-ref $env_t42 $env k15) (void))))
 (define-label
  $lambda46
  (lambda ($env $seq14 $loop15 k15)
    (app*
     (make-closure
      (lambda-label $lambda44)
      (make-env $env_t41 ($loop15 $loop15) ($seq14 $seq14)))
     (make-closure (lambda-label $lambda45) (make-env $env_t42 (k15 k15))))))
 (define-label
  $lambda47
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda48
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda47) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda49
  (lambda ($env rv26)
    (set-then! g$sum rv26 (app* (env-ref $env_t43 $env k25) (void)))))
 (define-label
  $lambda50
  (lambda ($env k25)
    ((cps +)
     g$sum
     1
     (make-closure (lambda-label $lambda49) (make-env $env_t43 (k25 k25))))))
 (define-label
  $lambda51
  (lambda ($env continue k24)
    (set-then!
     g$i
     (env-ref $env_t44 $env i14)
     (app* (make-closure (lambda-label $lambda50) (make-env $env_t39)) k24))))
 (define-label
  $lambda52
  (lambda ($env i14 k23)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     (make-closure (lambda-label $lambda51) (make-env $env_t44 (i14 i14)))
     k23)))
 (define-label
  $lambda53
  (lambda ($env rv22)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t39))
     rv22
     (make-closure (lambda-label $lambda52) (make-env $env_t39))
     (env-ref $env_t45 $env k14))))
 (define-label
  $lambda54
  (lambda ($env break k14)
    (app*
     g$range
     10
     (make-closure (lambda-label $lambda53) (make-env $env_t45 (k14 k14))))))
 (define-env $env_t45 (k14))
 (define-env $env_t38 (cc))
 (define-env $env_t40 ($loop15 $seq14 k16))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t42 (k15))
 (define-env $env_t39 ())
 (define-env $env_t43 (k25))
 (define-env $env_t44 (i14))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$sum (void))
 (define g$i (void))
 (app*
  (make-closure (lambda-label $lambda39) (make-env $env_t39))
  (make-closure (lambda-label $lambda54) (make-env $env_t39))
  $halt))
