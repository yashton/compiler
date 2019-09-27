(program
 (define-label
  $lambda40
  (lambda ($env rv17)
    (if rv17
      ((cps py-list-ref)
       (env-ref $env_t38 $env e16)
       (env-ref $env_t38 $env i15)
       (env-ref $env_t38 $env k16))
      ((cps tuple?)
       (env-ref $env_t38 $env e16)
       (make-closure
        (lambda-label $lambda39)
        (make-env
         $env_t38
         (e16 (env-ref $env_t38 $env e16))
         (i15 (env-ref $env_t38 $env i15))
         (k16 (env-ref $env_t38 $env k16))))))))
 (define-label
  $lambda41
  (lambda ($env i15 k16)
    ((cps py-list?)
     (env-ref $env_t39 $env e16)
     (make-closure
      (lambda-label $lambda40)
      (make-env
       $env_t38
       (e16 (env-ref $env_t39 $env e16))
       (i15 i15)
       (k16 k16))))))
 (define-label
  $lambda42
  (lambda ($env e16 k15)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39 (e16 e16)))
     0
     k15)))
 (define-label
  $lambda43
  (lambda ($env rv25)
    (if rv25
      ((cps dict-ref)
       (env-ref $env_t41 $env e18)
       (env-ref $env_t41 $env i17)
       (env-ref $env_t41 $env k22))
      (error "cannot index object" (env-ref $env_t41 $env k22)))))
 (define-label
  $lambda44
  (lambda ($env rv24)
    (if rv24
      ((cps tuple-ref)
       (env-ref $env_t41 $env e18)
       (env-ref $env_t41 $env i17)
       (env-ref $env_t41 $env k22))
      ((cps dict?)
       (env-ref $env_t41 $env e18)
       (make-closure
        (lambda-label $lambda43)
        (make-env
         $env_t41
         (e18 (env-ref $env_t41 $env e18))
         (i17 (env-ref $env_t41 $env i17))
         (k22 (env-ref $env_t41 $env k22))))))))
 (define-label
  $lambda45
  (lambda ($env rv23)
    (if rv23
      ((cps py-list-ref)
       (env-ref $env_t41 $env e18)
       (env-ref $env_t41 $env i17)
       (env-ref $env_t41 $env k22))
      ((cps tuple?)
       (env-ref $env_t41 $env e18)
       (make-closure
        (lambda-label $lambda44)
        (make-env
         $env_t41
         (e18 (env-ref $env_t41 $env e18))
         (i17 (env-ref $env_t41 $env i17))
         (k22 (env-ref $env_t41 $env k22))))))))
 (define-label
  $lambda46
  (lambda ($env i17 k22)
    ((cps py-list?)
     (env-ref $env_t42 $env e18)
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t41
       (e18 (env-ref $env_t42 $env e18))
       (i17 i17)
       (k22 k22))))))
 (define-label
  $lambda47
  (lambda ($env e18 k21)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t42 (e18 e18)))
     1
     k21)))
 (define-label
  $lambda48
  (lambda ($env rv26)
    (set-then! g$y rv26 (app* (env-ref $env_t43 $env k14) (void)))))
 (define-label
  $lambda49
  (lambda ($env rv20)
    (set-then!
     g$x
     rv20
     (app*
      (make-closure (lambda-label $lambda47) (make-env $env_t40))
      (env-ref $env_t44 $env t14)
      (make-closure
       (lambda-label $lambda48)
       (make-env $env_t43 (k14 (env-ref $env_t44 $env k14))))))))
 (define-label
  $lambda50
  (lambda ($env t14 k14)
    (app*
     (make-closure (lambda-label $lambda42) (make-env $env_t40))
     t14
     (make-closure
      (lambda-label $lambda49)
      (make-env $env_t44 (k14 k14) (t14 t14))))))
 (define-label $lambda51 (lambda ($env rv29) ((cps py-print) g$y $halt)))
 (define-label
  $lambda52
  (lambda ($env rv28)
    ((cps py-print)
     g$x
     (make-closure (lambda-label $lambda51) (make-env $env_t40)))))
 (define-label
  $lambda53
  (lambda ($env rv27)
    ((cps py-print)
     g$a
     (make-closure (lambda-label $lambda52) (make-env $env_t40)))))
 (define-label
  $lambda38
  (lambda ($env rv19)
    (if rv19
      ((cps dict-ref)
       (env-ref $env_t38 $env e16)
       (env-ref $env_t38 $env i15)
       (env-ref $env_t38 $env k16))
      (error "cannot index object" (env-ref $env_t38 $env k16)))))
 (define-label
  $lambda39
  (lambda ($env rv18)
    (if rv18
      ((cps tuple-ref)
       (env-ref $env_t38 $env e16)
       (env-ref $env_t38 $env i15)
       (env-ref $env_t38 $env k16))
      ((cps dict?)
       (env-ref $env_t38 $env e16)
       (make-closure
        (lambda-label $lambda38)
        (make-env
         $env_t38
         (e16 (env-ref $env_t38 $env e16))
         (i15 (env-ref $env_t38 $env i15))
         (k16 (env-ref $env_t38 $env k16))))))))
 (define-env $env_t41 (e18 i17 k22))
 (define-env $env_t42 (e18))
 (define-env $env_t43 (k14))
 (define-env $env_t44 (k14 t14))
 (define-env $env_t38 (e16 i15 k16))
 (define-env $env_t39 (e16))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$y (void))
 (define g$x (void))
 (set-then!
  g$a
  10
  (app*
   (make-closure (lambda-label $lambda50) (make-env $env_t40))
   (tuple 42 1701)
   (make-closure (lambda-label $lambda53) (make-env $env_t40)))))
