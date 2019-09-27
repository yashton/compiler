(program
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
  (lambda ($env rv31)
    (if rv31
      ((cps dict-ref)
       (env-ref $env_t43 $env e20)
       (env-ref $env_t43 $env i19)
       (env-ref $env_t43 $env k28))
      (error "cannot index object" (env-ref $env_t43 $env k28)))))
 (define-label
  $lambda49
  (lambda ($env rv30)
    (if rv30
      ((cps tuple-ref)
       (env-ref $env_t43 $env e20)
       (env-ref $env_t43 $env i19)
       (env-ref $env_t43 $env k28))
      ((cps dict?)
       (env-ref $env_t43 $env e20)
       (make-closure
        (lambda-label $lambda48)
        (make-env
         $env_t43
         (e20 (env-ref $env_t43 $env e20))
         (i19 (env-ref $env_t43 $env i19))
         (k28 (env-ref $env_t43 $env k28))))))))
 (define-label
  $lambda50
  (lambda ($env rv29)
    (if rv29
      ((cps py-list-ref)
       (env-ref $env_t43 $env e20)
       (env-ref $env_t43 $env i19)
       (env-ref $env_t43 $env k28))
      ((cps tuple?)
       (env-ref $env_t43 $env e20)
       (make-closure
        (lambda-label $lambda49)
        (make-env
         $env_t43
         (e20 (env-ref $env_t43 $env e20))
         (i19 (env-ref $env_t43 $env i19))
         (k28 (env-ref $env_t43 $env k28))))))))
 (define-label
  $lambda51
  (lambda ($env i19 k28)
    ((cps py-list?)
     (env-ref $env_t44 $env e20)
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t43
       (e20 (env-ref $env_t44 $env e20))
       (i19 i19)
       (k28 k28))))))
 (define-label
  $lambda52
  (lambda ($env e20 k27)
    (app*
     (make-closure (lambda-label $lambda51) (make-env $env_t44 (e20 e20)))
     2
     k27)))
 (define-label
  $lambda53
  (lambda ($env rv32)
    (set-then! g$z rv32 (app* (env-ref $env_t45 $env k14) (void)))))
 (define-label
  $lambda54
  (lambda ($env rv26)
    (set-then!
     g$y
     rv26
     (app*
      (make-closure (lambda-label $lambda52) (make-env $env_t40))
      (env-ref $env_t46 $env t14)
      (make-closure
       (lambda-label $lambda53)
       (make-env $env_t45 (k14 (env-ref $env_t46 $env k14))))))))
 (define-label
  $lambda55
  (lambda ($env rv20)
    (set-then!
     g$x
     rv20
     (app*
      (make-closure (lambda-label $lambda47) (make-env $env_t40))
      (env-ref $env_t46 $env t14)
      (make-closure
       (lambda-label $lambda54)
       (make-env
        $env_t46
        (k14 (env-ref $env_t46 $env k14))
        (t14 (env-ref $env_t46 $env t14))))))))
 (define-label
  $lambda56
  (lambda ($env t14 k14)
    (app*
     (make-closure (lambda-label $lambda42) (make-env $env_t40))
     t14
     (make-closure
      (lambda-label $lambda55)
      (make-env $env_t46 (k14 k14) (t14 t14))))))
 (define-label $lambda57 (lambda ($env rv35) ((cps py-print) g$z $halt)))
 (define-label
  $lambda58
  (lambda ($env rv34)
    ((cps py-print)
     g$y
     (make-closure (lambda-label $lambda57) (make-env $env_t40)))))
 (define-label
  $lambda59
  (lambda ($env rv33)
    ((cps py-print)
     g$x
     (make-closure (lambda-label $lambda58) (make-env $env_t40)))))
 (define-env $env_t43 (e20 i19 k28))
 (define-env $env_t44 (e20))
 (define-env $env_t45 (k14))
 (define-env $env_t46 (k14 t14))
 (define-env $env_t41 (e18 i17 k22))
 (define-env $env_t42 (e18))
 (define-env $env_t38 (e16 i15 k16))
 (define-env $env_t39 (e16))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$v (void))
 (define g$z (void))
 (define g$x (void))
 (define g$y (void))
 (set-then!
  g$v
  (tuple "a" "b" "e")
  (app*
   (make-closure (lambda-label $lambda56) (make-env $env_t40))
   g$v
   (make-closure (lambda-label $lambda59) (make-env $env_t40)))))
