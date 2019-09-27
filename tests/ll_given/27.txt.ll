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
  (lambda ($env rv40)
    (if rv40
      ((cps dict-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k37))
      (error "cannot index object" (env-ref $env_t43 $env k37)))))
 (define-label
  $lambda50
  (lambda ($env rv39)
    (if rv39
      ((cps tuple-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k37))
      ((cps dict?)
       (env-ref $env_t43 $env e19)
       (make-closure
        (lambda-label $lambda49)
        (make-env
         $env_t43
         (e19 (env-ref $env_t43 $env e19))
         (i18 (env-ref $env_t43 $env i18))
         (k37 (env-ref $env_t43 $env k37))))))))
 (define-label
  $lambda51
  (lambda ($env rv38)
    (if rv38
      ((cps py-list-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k37))
      ((cps tuple?)
       (env-ref $env_t43 $env e19)
       (make-closure
        (lambda-label $lambda50)
        (make-env
         $env_t43
         (e19 (env-ref $env_t43 $env e19))
         (i18 (env-ref $env_t43 $env i18))
         (k37 (env-ref $env_t43 $env k37))))))))
 (define-label
  $lambda52
  (lambda ($env i18 k37)
    ((cps py-list?)
     (env-ref $env_t44 $env e19)
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t43
       (e19 (env-ref $env_t44 $env e19))
       (i18 i18)
       (k37 k37))))))
 (define-label
  $lambda53
  (lambda ($env e19 k36)
    (app*
     (make-closure (lambda-label $lambda52) (make-env $env_t44 (e19 e19)))
     (env-ref $env_t45 $env i15)
     k36)))
 (define-label
  $lambda54
  (lambda ($env rv31)
    ((cps tuple-set!)
     (env-ref $env_t46 $env b16)
     (env-ref $env_t46 $env i15)
     rv31
     (env-ref $env_t46 $env k29))))
 (define-label
  $lambda55
  (lambda ($env rv33)
    ((cps py-list-set!)
     (env-ref $env_t46 $env b16)
     (env-ref $env_t46 $env i15)
     rv33
     (env-ref $env_t46 $env k29))))
 (define-label
  $lambda56
  (lambda ($env rv35)
    ((cps dict-set!)
     (env-ref $env_t46 $env b16)
     (env-ref $env_t46 $env i15)
     rv35
     (env-ref $env_t46 $env k29))))
 (define-label
  $lambda57
  (lambda ($env rv34)
    (if rv34
      ((cps +)
       (env-ref $env_t47 $env v17)
       1
       (make-closure
        (lambda-label $lambda56)
        (make-env
         $env_t46
         (b16 (env-ref $env_t47 $env b16))
         (i15 (env-ref $env_t47 $env i15))
         (k29 (env-ref $env_t47 $env k29)))))
      (app* (env-ref $env_t47 $env k29) (void)))))
 (define-label
  $lambda58
  (lambda ($env rv32)
    (if rv32
      ((cps +)
       (env-ref $env_t47 $env v17)
       1
       (make-closure
        (lambda-label $lambda55)
        (make-env
         $env_t46
         (b16 (env-ref $env_t47 $env b16))
         (i15 (env-ref $env_t47 $env i15))
         (k29 (env-ref $env_t47 $env k29)))))
      ((cps dict?)
       (env-ref $env_t47 $env b16)
       (make-closure
        (lambda-label $lambda57)
        (make-env
         $env_t47
         (b16 (env-ref $env_t47 $env b16))
         (i15 (env-ref $env_t47 $env i15))
         (k29 (env-ref $env_t47 $env k29))
         (v17 (env-ref $env_t47 $env v17))))))))
 (define-label
  $lambda59
  (lambda ($env rv30)
    (if rv30
      ((cps +)
       (env-ref $env_t47 $env v17)
       1
       (make-closure
        (lambda-label $lambda54)
        (make-env
         $env_t46
         (b16 (env-ref $env_t47 $env b16))
         (i15 (env-ref $env_t47 $env i15))
         (k29 (env-ref $env_t47 $env k29)))))
      ((cps py-list?)
       (env-ref $env_t47 $env b16)
       (make-closure
        (lambda-label $lambda58)
        (make-env
         $env_t47
         (b16 (env-ref $env_t47 $env b16))
         (i15 (env-ref $env_t47 $env i15))
         (k29 (env-ref $env_t47 $env k29))
         (v17 (env-ref $env_t47 $env v17))))))))
 (define-label
  $lambda60
  (lambda ($env v17 k29)
    ((cps tuple?)
     (env-ref $env_t48 $env b16)
     (make-closure
      (lambda-label $lambda59)
      (make-env
       $env_t47
       (b16 (env-ref $env_t48 $env b16))
       (i15 (env-ref $env_t48 $env i15))
       (k29 k29)
       (v17 v17))))))
 (define-label
  $lambda61
  (lambda ($env rv41)
    (app*
     (make-closure
      (lambda-label $lambda60)
      (make-env
       $env_t48
       (b16 (env-ref $env_t49 $env b16))
       (i15 (env-ref $env_t49 $env i15))))
     rv41
     (env-ref $env_t49 $env k28))))
 (define-label
  $lambda62
  (lambda ($env i15 k28)
    (app*
     (make-closure (lambda-label $lambda53) (make-env $env_t45 (i15 i15)))
     (env-ref $env_t50 $env b16)
     (make-closure
      (lambda-label $lambda61)
      (make-env
       $env_t49
       (b16 (env-ref $env_t50 $env b16))
       (i15 i15)
       (k28 k28))))))
 (define-label
  $lambda63
  (lambda ($env b16 k27)
    (app*
     (make-closure (lambda-label $lambda62) (make-env $env_t50 (b16 b16)))
     g$index
     k27)))
 (define-label
  $lambda64
  (lambda ($env k26)
    (app*
     (make-closure (lambda-label $lambda63) (make-env $env_t39))
     g$myList
     k26)))
 (define-label
  $lambda65
  (lambda ($env continue k25)
    (set-then!
     g$index
     (env-ref $env_t51 $env i14)
     (app* (make-closure (lambda-label $lambda64) (make-env $env_t39)) k25))))
 (define-label
  $lambda66
  (lambda ($env i14 k24)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     (make-closure (lambda-label $lambda65) (make-env $env_t51 (i14 i14)))
     k24)))
 (define-label
  $lambda67
  (lambda ($env rv23)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t39))
     rv23
     (make-closure (lambda-label $lambda66) (make-env $env_t39))
     (env-ref $env_t52 $env k14))))
 (define-label
  $lambda68
  (lambda ($env rv22)
    (app*
     g$range
     rv22
     (make-closure
      (lambda-label $lambda67)
      (make-env $env_t52 (k14 (env-ref $env_t52 $env k14)))))))
 (define-label
  $lambda69
  (lambda ($env break k14)
    (app*
     g$len
     g$myList
     (make-closure (lambda-label $lambda68) (make-env $env_t52 (k14 k14))))))
 (define-label $lambda70 (lambda ($env rv42) ((cps py-print) g$myList $halt)))
 (define-env $env_t47 (b16 i15 k29 v17))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t50 (b16))
 (define-env $env_t43 (e19 i18 k37))
 (define-env $env_t44 (e19))
 (define-env $env_t45 (i15))
 (define-env $env_t46 (b16 i15 k29))
 (define-env $env_t40 ($loop15 $seq14 k16))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t49 (b16 i15 k28))
 (define-env $env_t42 (k15))
 (define-env $env_t51 (i14))
 (define-env $env_t52 (k14))
 (define-env $env_t48 (b16 i15))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$index (void))
 (define g$myList (void))
 (set-then!
  g$myList
  (py-list* 1 2 3 4)
  (app*
   (make-closure (lambda-label $lambda39) (make-env $env_t39))
   (make-closure (lambda-label $lambda69) (make-env $env_t39))
   (make-closure (lambda-label $lambda70) (make-env $env_t39)))))
