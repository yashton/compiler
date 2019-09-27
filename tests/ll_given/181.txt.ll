(program
 (define-label
  $lambda62
  (lambda ($env rv28)
    ((cps py-list-set!)
     (env-ref $env_t49 $env b17)
     (env-ref $env_t49 $env i16)
     rv28
     (env-ref $env_t49 $env k24))))
 (define-label
  $lambda63
  (lambda ($env rv30)
    ((cps dict-set!)
     (env-ref $env_t49 $env b17)
     (env-ref $env_t49 $env i16)
     rv30
     (env-ref $env_t49 $env k24))))
 (define-label
  $lambda64
  (lambda ($env rv29)
    (if rv29
      ((cps +)
       (env-ref $env_t50 $env v18)
       30
       (make-closure
        (lambda-label $lambda63)
        (make-env
         $env_t49
         (b17 (env-ref $env_t50 $env b17))
         (i16 (env-ref $env_t50 $env i16))
         (k24 (env-ref $env_t50 $env k24)))))
      (app* (env-ref $env_t50 $env k24) (void)))))
 (define-label
  $lambda65
  (lambda ($env rv27)
    (if rv27
      ((cps +)
       (env-ref $env_t50 $env v18)
       30
       (make-closure
        (lambda-label $lambda62)
        (make-env
         $env_t49
         (b17 (env-ref $env_t50 $env b17))
         (i16 (env-ref $env_t50 $env i16))
         (k24 (env-ref $env_t50 $env k24)))))
      ((cps dict?)
       (env-ref $env_t50 $env b17)
       (make-closure
        (lambda-label $lambda64)
        (make-env
         $env_t50
         (b17 (env-ref $env_t50 $env b17))
         (i16 (env-ref $env_t50 $env i16))
         (k24 (env-ref $env_t50 $env k24))
         (v18 (env-ref $env_t50 $env v18))))))))
 (define-label
  $lambda66
  (lambda ($env rv25)
    (if rv25
      ((cps +)
       (env-ref $env_t50 $env v18)
       30
       (make-closure
        (lambda-label $lambda61)
        (make-env
         $env_t49
         (b17 (env-ref $env_t50 $env b17))
         (i16 (env-ref $env_t50 $env i16))
         (k24 (env-ref $env_t50 $env k24)))))
      ((cps py-list?)
       (env-ref $env_t50 $env b17)
       (make-closure
        (lambda-label $lambda65)
        (make-env
         $env_t50
         (b17 (env-ref $env_t50 $env b17))
         (i16 (env-ref $env_t50 $env i16))
         (k24 (env-ref $env_t50 $env k24))
         (v18 (env-ref $env_t50 $env v18))))))))
 (define-label
  $lambda67
  (lambda ($env v18 k24)
    ((cps tuple?)
     (env-ref $env_t51 $env b17)
     (make-closure
      (lambda-label $lambda66)
      (make-env
       $env_t50
       (b17 (env-ref $env_t51 $env b17))
       (i16 (env-ref $env_t51 $env i16))
       (k24 k24)
       (v18 v18))))))
 (define-label
  $lambda68
  (lambda ($env rv36)
    (app*
     (make-closure
      (lambda-label $lambda67)
      (make-env
       $env_t51
       (b17 (env-ref $env_t52 $env b17))
       (i16 (env-ref $env_t52 $env i16))))
     rv36
     (env-ref $env_t52 $env k23))))
 (define-label
  $lambda69
  (lambda ($env i16 k23)
    (app*
     (make-closure (lambda-label $lambda60) (make-env $env_t48 (i16 i16)))
     (env-ref $env_t53 $env b17)
     (make-closure
      (lambda-label $lambda68)
      (make-env
       $env_t52
       (b17 (env-ref $env_t53 $env b17))
       (i16 i16)
       (k23 k23))))))
 (define-label
  $lambda70
  (lambda ($env rv37)
    (app*
     (make-closure
      (lambda-label $lambda69)
      (make-env $env_t53 (b17 (env-ref $env_t54 $env b17))))
     rv37
     (env-ref $env_t54 $env k22))))
 (define-label
  $lambda71
  (lambda ($env b17 k22)
    (app*
     g$g
     (make-closure
      (lambda-label $lambda70)
      (make-env $env_t54 (b17 b17) (k22 k22))))))
 (define-label
  $lambda72
  (lambda ($env rv50)
    (if rv50
      ((cps dict-ref)
       (env-ref $env_t55 $env e22)
       (env-ref $env_t55 $env i21)
       (env-ref $env_t55 $env k47))
      (error "cannot index object" (env-ref $env_t55 $env k47)))))
 (define-label
  $lambda73
  (lambda ($env rv49)
    (if rv49
      ((cps tuple-ref)
       (env-ref $env_t55 $env e22)
       (env-ref $env_t55 $env i21)
       (env-ref $env_t55 $env k47))
      ((cps dict?)
       (env-ref $env_t55 $env e22)
       (make-closure
        (lambda-label $lambda72)
        (make-env
         $env_t55
         (e22 (env-ref $env_t55 $env e22))
         (i21 (env-ref $env_t55 $env i21))
         (k47 (env-ref $env_t55 $env k47))))))))
 (define-label
  $lambda74
  (lambda ($env rv48)
    (if rv48
      ((cps py-list-ref)
       (env-ref $env_t55 $env e22)
       (env-ref $env_t55 $env i21)
       (env-ref $env_t55 $env k47))
      ((cps tuple?)
       (env-ref $env_t55 $env e22)
       (make-closure
        (lambda-label $lambda73)
        (make-env
         $env_t55
         (e22 (env-ref $env_t55 $env e22))
         (i21 (env-ref $env_t55 $env i21))
         (k47 (env-ref $env_t55 $env k47))))))))
 (define-label
  $lambda75
  (lambda ($env i21 k47)
    ((cps py-list?)
     (env-ref $env_t56 $env e22)
     (make-closure
      (lambda-label $lambda74)
      (make-env
       $env_t55
       (e22 (env-ref $env_t56 $env e22))
       (i21 i21)
       (k47 k47))))))
 (define-label
  $lambda76
  (lambda ($env rv51)
    (app*
     (make-closure
      (lambda-label $lambda75)
      (make-env $env_t56 (e22 (env-ref $env_t57 $env e22))))
     rv51
     (env-ref $env_t57 $env k46))))
 (define-label
  $lambda77
  (lambda ($env e22 k46)
    (app*
     g$f
     (make-closure
      (lambda-label $lambda76)
      (make-env $env_t57 (e22 e22) (k46 k46))))))
 (define-label $lambda78 (lambda ($env rv52) ((cps py-print) rv52 $halt)))
 (define-label
  $lambda79
  (lambda ($env rv45)
    (app*
     (make-closure (lambda-label $lambda77) (make-env $env_t39))
     g$a
     (make-closure (lambda-label $lambda78) (make-env $env_t39)))))
 (define-label
  $lambda80
  (lambda ($env rv44)
    (app*
     (make-closure (lambda-label $lambda71) (make-env $env_t39))
     rv44
     (make-closure (lambda-label $lambda79) (make-env $env_t39)))))
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
  (lambda ($env rv17)
    (app* (env-ref $env_t40 $env return) 1 (env-ref $env_t40 $env k16))))
 (define-label
  $lambda41
  (lambda ($env k16)
    ((cps py-print)
     "called f"
     (make-closure
      (lambda-label $lambda40)
      (make-env $env_t40 (k16 k16) (return (env-ref $env_t41 $env return)))))))
 (define-label
  $lambda42
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda41)
      (make-env $env_t41 (return return)))
     k15)))
 (define-label
  $lambda43
  (lambda ($env k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda42) (make-env $env_t39))
     k14)))
 (define-label
  $lambda44
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda45
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda44) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda46
  (lambda ($env rv21)
    (app* (env-ref $env_t42 $env return) 0 (env-ref $env_t42 $env k20))))
 (define-label
  $lambda47
  (lambda ($env k20)
    ((cps py-print)
     "called g"
     (make-closure
      (lambda-label $lambda46)
      (make-env $env_t42 (k20 k20) (return (env-ref $env_t41 $env return)))))))
 (define-label
  $lambda48
  (lambda ($env return k19)
    (app*
     (make-closure
      (lambda-label $lambda47)
      (make-env $env_t41 (return return)))
     k19)))
 (define-label
  $lambda49
  (lambda ($env k18)
    (app*
     (make-closure (lambda-label $lambda45) (make-env $env_t39))
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     k18)))
 (define-label
  $lambda50
  (lambda ($env rv42)
    (if rv42
      ((cps dict-ref)
       (env-ref $env_t43 $env e15)
       (env-ref $env_t43 $env i14)
       (env-ref $env_t43 $env k39))
      (error "cannot index object" (env-ref $env_t43 $env k39)))))
 (define-label
  $lambda51
  (lambda ($env rv41)
    (if rv41
      ((cps tuple-ref)
       (env-ref $env_t43 $env e15)
       (env-ref $env_t43 $env i14)
       (env-ref $env_t43 $env k39))
      ((cps dict?)
       (env-ref $env_t43 $env e15)
       (make-closure
        (lambda-label $lambda50)
        (make-env
         $env_t43
         (e15 (env-ref $env_t43 $env e15))
         (i14 (env-ref $env_t43 $env i14))
         (k39 (env-ref $env_t43 $env k39))))))))
 (define-label
  $lambda52
  (lambda ($env rv40)
    (if rv40
      ((cps py-list-ref)
       (env-ref $env_t43 $env e15)
       (env-ref $env_t43 $env i14)
       (env-ref $env_t43 $env k39))
      ((cps tuple?)
       (env-ref $env_t43 $env e15)
       (make-closure
        (lambda-label $lambda51)
        (make-env
         $env_t43
         (e15 (env-ref $env_t43 $env e15))
         (i14 (env-ref $env_t43 $env i14))
         (k39 (env-ref $env_t43 $env k39))))))))
 (define-label
  $lambda53
  (lambda ($env i14 k39)
    ((cps py-list?)
     (env-ref $env_t44 $env e15)
     (make-closure
      (lambda-label $lambda52)
      (make-env
       $env_t43
       (e15 (env-ref $env_t44 $env e15))
       (i14 i14)
       (k39 k39))))))
 (define-label
  $lambda54
  (lambda ($env rv43)
    (app*
     (make-closure
      (lambda-label $lambda53)
      (make-env $env_t44 (e15 (env-ref $env_t45 $env e15))))
     rv43
     (env-ref $env_t45 $env k38))))
 (define-label
  $lambda55
  (lambda ($env e15 k38)
    (app*
     g$f
     (make-closure
      (lambda-label $lambda54)
      (make-env $env_t45 (e15 e15) (k38 k38))))))
 (define-label
  $lambda56
  (lambda ($env rv35)
    (if rv35
      ((cps dict-ref)
       (env-ref $env_t46 $env e20)
       (env-ref $env_t46 $env i19)
       (env-ref $env_t46 $env k32))
      (error "cannot index object" (env-ref $env_t46 $env k32)))))
 (define-label
  $lambda57
  (lambda ($env rv34)
    (if rv34
      ((cps tuple-ref)
       (env-ref $env_t46 $env e20)
       (env-ref $env_t46 $env i19)
       (env-ref $env_t46 $env k32))
      ((cps dict?)
       (env-ref $env_t46 $env e20)
       (make-closure
        (lambda-label $lambda56)
        (make-env
         $env_t46
         (e20 (env-ref $env_t46 $env e20))
         (i19 (env-ref $env_t46 $env i19))
         (k32 (env-ref $env_t46 $env k32))))))))
 (define-label
  $lambda58
  (lambda ($env rv33)
    (if rv33
      ((cps py-list-ref)
       (env-ref $env_t46 $env e20)
       (env-ref $env_t46 $env i19)
       (env-ref $env_t46 $env k32))
      ((cps tuple?)
       (env-ref $env_t46 $env e20)
       (make-closure
        (lambda-label $lambda57)
        (make-env
         $env_t46
         (e20 (env-ref $env_t46 $env e20))
         (i19 (env-ref $env_t46 $env i19))
         (k32 (env-ref $env_t46 $env k32))))))))
 (define-label
  $lambda59
  (lambda ($env i19 k32)
    ((cps py-list?)
     (env-ref $env_t47 $env e20)
     (make-closure
      (lambda-label $lambda58)
      (make-env
       $env_t46
       (e20 (env-ref $env_t47 $env e20))
       (i19 i19)
       (k32 k32))))))
 (define-label
  $lambda60
  (lambda ($env e20 k31)
    (app*
     (make-closure (lambda-label $lambda59) (make-env $env_t47 (e20 e20)))
     (env-ref $env_t48 $env i16)
     k31)))
 (define-label
  $lambda61
  (lambda ($env rv26)
    ((cps tuple-set!)
     (env-ref $env_t49 $env b17)
     (env-ref $env_t49 $env i16)
     rv26
     (env-ref $env_t49 $env k24))))
 (define-env $env_t40 (k16 return))
 (define-env $env_t41 (return))
 (define-env $env_t55 (e22 i21 k47))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t54 (b17 k22))
 (define-env $env_t46 (e20 i19 k32))
 (define-env $env_t47 (e20))
 (define-env $env_t48 (i16))
 (define-env $env_t57 (e22 k46))
 (define-env $env_t42 (k20 return))
 (define-env $env_t49 (b17 i16 k24))
 (define-env $env_t53 (b17))
 (define-env $env_t50 (b17 i16 k24 v18))
 (define-env $env_t51 (b17 i16))
 (define-env $env_t52 (b17 i16 k23))
 (define-env $env_t43 (e15 i14 k39))
 (define-env $env_t44 (e15))
 (define-env $env_t45 (e15 k38))
 (define-env $env_t56 (e22))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$g (void))
 (define g$a (void))
 (define g$f (void))
 (set-then!
  g$f
  (make-closure (lambda-label $lambda43) (make-env $env_t39))
  (set-then!
   g$g
   (make-closure (lambda-label $lambda49) (make-env $env_t39))
   (set-then!
    g$a
    (py-list* (py-list* 10 20) (py-list* 30 40) (py-list* 50 60))
    (app*
     (make-closure (lambda-label $lambda55) (make-env $env_t39))
     g$a
     (make-closure (lambda-label $lambda80) (make-env $env_t39)))))))
