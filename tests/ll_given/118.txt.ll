(program
 (define-label
  $lambda65
  (lambda ($env rv27)
    (if rv27
      ((cps -)
       (env-ref $env_t48 $env n)
       1
       (make-closure
        (lambda-label $lambda58)
        (make-env
         $env_t48
         (b17 (env-ref $env_t48 $env b17))
         (i16 (env-ref $env_t48 $env i16))
         (k20 (env-ref $env_t48 $env k20))
         (n (env-ref $env_t48 $env n)))))
      ((cps dict?)
       (env-ref $env_t48 $env b17)
       (make-closure
        (lambda-label $lambda64)
        (make-env
         $env_t48
         (b17 (env-ref $env_t48 $env b17))
         (i16 (env-ref $env_t48 $env i16))
         (k20 (env-ref $env_t48 $env k20))
         (n (env-ref $env_t48 $env n))))))))
 (define-label
  $lambda66
  (lambda ($env rv21)
    (if rv21
      ((cps -)
       (env-ref $env_t48 $env n)
       1
       (make-closure
        (lambda-label $lambda53)
        (make-env
         $env_t48
         (b17 (env-ref $env_t48 $env b17))
         (i16 (env-ref $env_t48 $env i16))
         (k20 (env-ref $env_t48 $env k20))
         (n (env-ref $env_t48 $env n)))))
      ((cps py-list?)
       (env-ref $env_t48 $env b17)
       (make-closure
        (lambda-label $lambda65)
        (make-env
         $env_t48
         (b17 (env-ref $env_t48 $env b17))
         (i16 (env-ref $env_t48 $env i16))
         (k20 (env-ref $env_t48 $env k20))
         (n (env-ref $env_t48 $env n))))))))
 (define-label
  $lambda67
  (lambda ($env i16 k20)
    ((cps tuple?)
     (env-ref $env_t51 $env b17)
     (make-closure
      (lambda-label $lambda66)
      (make-env
       $env_t48
       (b17 (env-ref $env_t51 $env b17))
       (i16 i16)
       (k20 k20)
       (n (env-ref $env_t51 $env n)))))))
 (define-label
  $lambda68
  (lambda ($env b17 k19)
    (app*
     (make-closure
      (lambda-label $lambda67)
      (make-env $env_t51 (b17 b17) (n (env-ref $env_t42 $env n))))
     (env-ref $env_t42 $env n)
     k19)))
 (define-label
  $lambda69
  (lambda ($env rv44)
    (if rv44
      ((cps dict-ref)
       (env-ref $env_t52 $env e19)
       (env-ref $env_t52 $env i18)
       (env-ref $env_t52 $env k41))
      (error "cannot index object" (env-ref $env_t52 $env k41)))))
 (define-label
  $lambda70
  (lambda ($env rv43)
    (if rv43
      ((cps tuple-ref)
       (env-ref $env_t52 $env e19)
       (env-ref $env_t52 $env i18)
       (env-ref $env_t52 $env k41))
      ((cps dict?)
       (env-ref $env_t52 $env e19)
       (make-closure
        (lambda-label $lambda69)
        (make-env
         $env_t52
         (e19 (env-ref $env_t52 $env e19))
         (i18 (env-ref $env_t52 $env i18))
         (k41 (env-ref $env_t52 $env k41))))))))
 (define-label
  $lambda71
  (lambda ($env rv42)
    (if rv42
      ((cps py-list-ref)
       (env-ref $env_t52 $env e19)
       (env-ref $env_t52 $env i18)
       (env-ref $env_t52 $env k41))
      ((cps tuple?)
       (env-ref $env_t52 $env e19)
       (make-closure
        (lambda-label $lambda70)
        (make-env
         $env_t52
         (e19 (env-ref $env_t52 $env e19))
         (i18 (env-ref $env_t52 $env i18))
         (k41 (env-ref $env_t52 $env k41))))))))
 (define-label
  $lambda72
  (lambda ($env i18 k41)
    ((cps py-list?)
     (env-ref $env_t53 $env e19)
     (make-closure
      (lambda-label $lambda71)
      (make-env
       $env_t52
       (e19 (env-ref $env_t53 $env e19))
       (i18 i18)
       (k41 k41))))))
 (define-label
  $lambda73
  (lambda ($env e19 k40)
    (app*
     (make-closure (lambda-label $lambda72) (make-env $env_t53 (e19 e19)))
     (env-ref $env_t42 $env n)
     k40)))
 (define-label
  $lambda74
  (lambda ($env rv45)
    (app* (env-ref $env_t54 $env return) rv45 (env-ref $env_t54 $env k16))))
 (define-label
  $lambda75
  (lambda ($env rv39)
    (app*
     (make-closure
      (lambda-label $lambda73)
      (make-env $env_t42 (n (env-ref $env_t55 $env n))))
     g$cache
     (make-closure
      (lambda-label $lambda74)
      (make-env
       $env_t54
       (k16 (env-ref $env_t55 $env k16))
       (return (env-ref $env_t55 $env return)))))))
 (define-label
  $lambda76
  (lambda ($env rv18)
    (app*
     (make-closure
      (lambda-label $lambda68)
      (make-env $env_t42 (n (env-ref $env_t55 $env n))))
     g$cache
     (make-closure
      (lambda-label $lambda75)
      (make-env
       $env_t55
       (k16 (env-ref $env_t55 $env k16))
       (n (env-ref $env_t55 $env n))
       (return (env-ref $env_t55 $env return)))))))
 (define-label
  $lambda77
  (lambda ($env k16)
    (app*
     (make-closure
      (lambda-label $lambda48)
      (make-env
       $env_t44
       (n (env-ref $env_t44 $env n))
       (return (env-ref $env_t44 $env return))))
     (make-closure
      (lambda-label $lambda76)
      (make-env
       $env_t55
       (k16 k16)
       (n (env-ref $env_t44 $env n))
       (return (env-ref $env_t44 $env return)))))))
 (define-label
  $lambda78
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda77)
      (make-env $env_t44 (n (env-ref $env_t42 $env n)) (return return)))
     k15)))
 (define-label
  $lambda79
  (lambda ($env n k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda78) (make-env $env_t42 (n n)))
     k14)))
 (define-label $lambda80 (lambda ($env rv54) ((cps py-print) rv54 $halt)))
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
  (lambda ($env rv52)
    (if rv52
      ((cps dict-ref)
       (env-ref $env_t40 $env e15)
       (env-ref $env_t40 $env i14)
       (env-ref $env_t40 $env k49))
      (error "cannot index object" (env-ref $env_t40 $env k49)))))
 (define-label
  $lambda41
  (lambda ($env rv51)
    (if rv51
      ((cps tuple-ref)
       (env-ref $env_t40 $env e15)
       (env-ref $env_t40 $env i14)
       (env-ref $env_t40 $env k49))
      ((cps dict?)
       (env-ref $env_t40 $env e15)
       (make-closure
        (lambda-label $lambda40)
        (make-env
         $env_t40
         (e15 (env-ref $env_t40 $env e15))
         (i14 (env-ref $env_t40 $env i14))
         (k49 (env-ref $env_t40 $env k49))))))))
 (define-label
  $lambda42
  (lambda ($env rv50)
    (if rv50
      ((cps py-list-ref)
       (env-ref $env_t40 $env e15)
       (env-ref $env_t40 $env i14)
       (env-ref $env_t40 $env k49))
      ((cps tuple?)
       (env-ref $env_t40 $env e15)
       (make-closure
        (lambda-label $lambda41)
        (make-env
         $env_t40
         (e15 (env-ref $env_t40 $env e15))
         (i14 (env-ref $env_t40 $env i14))
         (k49 (env-ref $env_t40 $env k49))))))))
 (define-label
  $lambda43
  (lambda ($env i14 k49)
    ((cps py-list?)
     (env-ref $env_t41 $env e15)
     (make-closure
      (lambda-label $lambda42)
      (make-env
       $env_t40
       (e15 (env-ref $env_t41 $env e15))
       (i14 i14)
       (k49 k49))))))
 (define-label
  $lambda44
  (lambda ($env e15 k48)
    (app*
     (make-closure (lambda-label $lambda43) (make-env $env_t41 (e15 e15)))
     (env-ref $env_t42 $env n)
     k48)))
 (define-label
  $lambda45
  (lambda ($env rv53)
    (app* (env-ref $env_t43 $env return) rv53 (env-ref $env_t43 $env k47))))
 (define-label
  $lambda46
  (lambda ($env k47)
    (app*
     (make-closure
      (lambda-label $lambda44)
      (make-env $env_t42 (n (env-ref $env_t44 $env n))))
     g$cache
     (make-closure
      (lambda-label $lambda45)
      (make-env $env_t43 (k47 k47) (return (env-ref $env_t44 $env return)))))))
 (define-label
  $lambda47
  (lambda ($env rv46)
    (if rv46
      (app*
       (make-closure
        (lambda-label $lambda46)
        (make-env
         $env_t44
         (n (env-ref $env_t45 $env n))
         (return (env-ref $env_t45 $env return))))
       (env-ref $env_t45 $env k17))
      (app* (env-ref $env_t45 $env k17) (void)))))
 (define-label
  $lambda48
  (lambda ($env k17)
    ((cps in?)
     (env-ref $env_t44 $env n)
     g$cache
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t45
       (k17 k17)
       (n (env-ref $env_t44 $env n))
       (return (env-ref $env_t44 $env return)))))))
 (define-label
  $lambda49
  (lambda ($env rv26)
    ((cps tuple-set!)
     (env-ref $env_t46 $env b17)
     (env-ref $env_t46 $env i16)
     rv26
     (env-ref $env_t46 $env k20))))
 (define-label
  $lambda50
  (lambda ($env rv25)
    ((cps +)
     (env-ref $env_t47 $env rv23)
     rv25
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t46
       (b17 (env-ref $env_t47 $env b17))
       (i16 (env-ref $env_t47 $env i16))
       (k20 (env-ref $env_t47 $env k20)))))))
 (define-label
  $lambda51
  (lambda ($env rv24)
    (app*
     g$fib
     rv24
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t47
       (b17 (env-ref $env_t47 $env b17))
       (i16 (env-ref $env_t47 $env i16))
       (k20 (env-ref $env_t47 $env k20))
       (rv23 (env-ref $env_t47 $env rv23)))))))
 (define-label
  $lambda52
  (lambda ($env rv23)
    ((cps -)
     (env-ref $env_t48 $env n)
     2
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t47
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (rv23 rv23))))))
 (define-label
  $lambda53
  (lambda ($env rv22)
    (app*
     g$fib
     rv22
     (make-closure
      (lambda-label $lambda52)
      (make-env
       $env_t48
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (n (env-ref $env_t48 $env n)))))))
 (define-label
  $lambda54
  (lambda ($env rv32)
    ((cps py-list-set!)
     (env-ref $env_t46 $env b17)
     (env-ref $env_t46 $env i16)
     rv32
     (env-ref $env_t46 $env k20))))
 (define-label
  $lambda55
  (lambda ($env rv31)
    ((cps +)
     (env-ref $env_t49 $env rv29)
     rv31
     (make-closure
      (lambda-label $lambda54)
      (make-env
       $env_t46
       (b17 (env-ref $env_t49 $env b17))
       (i16 (env-ref $env_t49 $env i16))
       (k20 (env-ref $env_t49 $env k20)))))))
 (define-label
  $lambda56
  (lambda ($env rv30)
    (app*
     g$fib
     rv30
     (make-closure
      (lambda-label $lambda55)
      (make-env
       $env_t49
       (b17 (env-ref $env_t49 $env b17))
       (i16 (env-ref $env_t49 $env i16))
       (k20 (env-ref $env_t49 $env k20))
       (rv29 (env-ref $env_t49 $env rv29)))))))
 (define-label
  $lambda57
  (lambda ($env rv29)
    ((cps -)
     (env-ref $env_t48 $env n)
     2
     (make-closure
      (lambda-label $lambda56)
      (make-env
       $env_t49
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (rv29 rv29))))))
 (define-label
  $lambda58
  (lambda ($env rv28)
    (app*
     g$fib
     rv28
     (make-closure
      (lambda-label $lambda57)
      (make-env
       $env_t48
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (n (env-ref $env_t48 $env n)))))))
 (define-label
  $lambda59
  (lambda ($env rv38)
    ((cps dict-set!)
     (env-ref $env_t46 $env b17)
     (env-ref $env_t46 $env i16)
     rv38
     (env-ref $env_t46 $env k20))))
 (define-label
  $lambda60
  (lambda ($env rv37)
    ((cps +)
     (env-ref $env_t50 $env rv35)
     rv37
     (make-closure
      (lambda-label $lambda59)
      (make-env
       $env_t46
       (b17 (env-ref $env_t50 $env b17))
       (i16 (env-ref $env_t50 $env i16))
       (k20 (env-ref $env_t50 $env k20)))))))
 (define-label
  $lambda61
  (lambda ($env rv36)
    (app*
     g$fib
     rv36
     (make-closure
      (lambda-label $lambda60)
      (make-env
       $env_t50
       (b17 (env-ref $env_t50 $env b17))
       (i16 (env-ref $env_t50 $env i16))
       (k20 (env-ref $env_t50 $env k20))
       (rv35 (env-ref $env_t50 $env rv35)))))))
 (define-label
  $lambda62
  (lambda ($env rv35)
    ((cps -)
     (env-ref $env_t48 $env n)
     2
     (make-closure
      (lambda-label $lambda61)
      (make-env
       $env_t50
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (rv35 rv35))))))
 (define-label
  $lambda63
  (lambda ($env rv34)
    (app*
     g$fib
     rv34
     (make-closure
      (lambda-label $lambda62)
      (make-env
       $env_t48
       (b17 (env-ref $env_t48 $env b17))
       (i16 (env-ref $env_t48 $env i16))
       (k20 (env-ref $env_t48 $env k20))
       (n (env-ref $env_t48 $env n)))))))
 (define-label
  $lambda64
  (lambda ($env rv33)
    (if rv33
      ((cps -)
       (env-ref $env_t48 $env n)
       1
       (make-closure
        (lambda-label $lambda63)
        (make-env
         $env_t48
         (b17 (env-ref $env_t48 $env b17))
         (i16 (env-ref $env_t48 $env i16))
         (k20 (env-ref $env_t48 $env k20))
         (n (env-ref $env_t48 $env n)))))
      (app* (env-ref $env_t48 $env k20) (void)))))
 (define-env $env_t52 (e19 i18 k41))
 (define-env $env_t53 (e19))
 (define-env $env_t54 (k16 return))
 (define-env $env_t51 (b17 n))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t47 (b17 i16 k20 rv23))
 (define-env $env_t46 (b17 i16 k20))
 (define-env $env_t55 (k16 n return))
 (define-env $env_t48 (b17 i16 k20 n))
 (define-env $env_t49 (b17 i16 k20 rv29))
 (define-env $env_t40 (e15 i14 k49))
 (define-env $env_t41 (e15))
 (define-env $env_t42 (n))
 (define-env $env_t50 (b17 i16 k20 rv35))
 (define-env $env_t43 (k47 return))
 (define-env $env_t44 (n return))
 (define-env $env_t45 (k17 n return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fib (void))
 (define g$cache (void))
 (set-then!
  g$cache
  (dict (0 0) (1 1))
  (set-then!
   g$fib
   (make-closure (lambda-label $lambda79) (make-env $env_t39))
   (app*
    g$fib
    25
    (make-closure (lambda-label $lambda80) (make-env $env_t39))))))
