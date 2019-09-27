(program
 (define-label
  $lambda60
  (lambda ($env rv46)
    (app*
     (make-closure
      (lambda-label $lambda59)
      (make-env $env_t47 (e18 (env-ref $env_t48 $env e18))))
     rv46
     (env-ref $env_t48 $env k41))))
 (define-label
  $lambda61
  (lambda ($env e18 k41)
    ((cps +)
     (get-cell (env-ref $env_t45 $env i))
     1
     (make-closure
      (lambda-label $lambda60)
      (make-env $env_t48 (e18 e18) (k41 k41))))))
 (define-label
  $lambda62
  (lambda ($env rv47) ((cps equal?) rv47 13 (env-ref $env_t49 $env k31))))
 (define-label
  $lambda63
  (lambda ($env rv40)
    (if rv40
      (app*
       (make-closure
        (lambda-label $lambda61)
        (make-env $env_t45 (i (env-ref $env_t50 $env i))))
       (env-ref $env_t50 $env nums)
       (make-closure
        (lambda-label $lambda62)
        (make-env $env_t49 (k31 (env-ref $env_t50 $env k31)))))
      (app* (env-ref $env_t50 $env k31) #f))))
 (define-label
  $lambda64
  (lambda ($env rv39)
    ((cps equal?)
     rv39
     13
     (make-closure
      (lambda-label $lambda63)
      (make-env
       $env_t50
       (i (env-ref $env_t50 $env i))
       (k31 (env-ref $env_t50 $env k31))
       (nums (env-ref $env_t50 $env nums)))))))
 (define-label
  $lambda65
  (lambda ($env k31)
    (app*
     (make-closure
      (lambda-label $lambda55)
      (make-env $env_t45 (i (env-ref $env_t51 $env i))))
     (env-ref $env_t51 $env nums)
     (make-closure
      (lambda-label $lambda64)
      (make-env
       $env_t50
       (i (env-ref $env_t51 $env i))
       (k31 k31)
       (nums (env-ref $env_t51 $env nums)))))))
 (define-label
  $lambda66
  (lambda ($env k33) (app* (env-ref $env_t52 $env return) #t k33)))
 (define-label
  $lambda67
  (lambda ($env rv32)
    (if rv32
      (app*
       (make-closure
        (lambda-label $lambda66)
        (make-env $env_t52 (return (env-ref $env_t53 $env return))))
       (env-ref $env_t53 $env k30))
      (app* (env-ref $env_t53 $env k30) (void)))))
 (define-label
  $lambda68
  (lambda ($env k30)
    (app*
     (make-closure
      (lambda-label $lambda65)
      (make-env
       $env_t51
       (i (env-ref $env_t54 $env i))
       (nums (env-ref $env_t54 $env nums))))
     (make-closure
      (lambda-label $lambda67)
      (make-env $env_t53 (k30 k30) (return (env-ref $env_t54 $env return)))))))
 (define-label
  $lambda69
  (lambda ($env continue k29)
    (set-cell!
     (env-ref $env_t55 $env i)
     (env-ref $env_t55 $env i14)
     (app*
      (make-closure
       (lambda-label $lambda68)
       (make-env
        $env_t54
        (i (env-ref $env_t55 $env i))
        (nums (env-ref $env_t55 $env nums))
        (return (env-ref $env_t55 $env return))))
      k29))))
 (define-label
  $lambda70
  (lambda ($env i14 k28)
    (app*
     (make-closure (lambda-label $lambda50) (make-env $env_t39))
     (make-closure
      (lambda-label $lambda69)
      (make-env
       $env_t55
       (i (env-ref $env_t54 $env i))
       (i14 i14)
       (nums (env-ref $env_t54 $env nums))
       (return (env-ref $env_t54 $env return))))
     k28)))
 (define-label
  $lambda71
  (lambda ($env rv27)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     rv27
     (make-closure
      (lambda-label $lambda70)
      (make-env
       $env_t54
       (i (env-ref $env_t56 $env i))
       (nums (env-ref $env_t56 $env nums))
       (return (env-ref $env_t56 $env return))))
     (env-ref $env_t56 $env k17))))
 (define-label
  $lambda72
  (lambda ($env rv26)
    (app*
     g$range
     rv26
     (make-closure
      (lambda-label $lambda71)
      (make-env
       $env_t56
       (i (env-ref $env_t56 $env i))
       (k17 (env-ref $env_t56 $env k17))
       (nums (env-ref $env_t56 $env nums))
       (return (env-ref $env_t56 $env return)))))))
 (define-label
  $lambda73
  (lambda ($env rv25)
    ((cps -)
     rv25
     1
     (make-closure
      (lambda-label $lambda72)
      (make-env
       $env_t56
       (i (env-ref $env_t56 $env i))
       (k17 (env-ref $env_t56 $env k17))
       (nums (env-ref $env_t56 $env nums))
       (return (env-ref $env_t56 $env return)))))))
 (define-label
  $lambda74
  (lambda ($env break k17)
    (app*
     g$len
     (env-ref $env_t54 $env nums)
     (make-closure
      (lambda-label $lambda73)
      (make-env
       $env_t56
       (i (env-ref $env_t54 $env i))
       (k17 k17)
       (nums (env-ref $env_t54 $env nums))
       (return (env-ref $env_t54 $env return)))))))
 (define-label
  $lambda75
  (lambda ($env rv48)
    (app* (env-ref $env_t57 $env return) #f (env-ref $env_t57 $env k16))))
 (define-label
  $lambda76
  (lambda ($env i k16)
    (set-then!
     i
     (make-cell i)
     (app*
      (make-closure (lambda-label $lambda41) (make-env $env_t39))
      (make-closure
       (lambda-label $lambda74)
       (make-env
        $env_t54
        (i i)
        (nums (env-ref $env_t58 $env nums))
        (return (env-ref $env_t58 $env return))))
      (make-closure
       (lambda-label $lambda75)
       (make-env
        $env_t57
        (k16 k16)
        (return (env-ref $env_t58 $env return))))))))
 (define-label
  $lambda77
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda76)
      (make-env $env_t58 (nums (env-ref $env_t59 $env nums)) (return return)))
     (void)
     k15)))
 (define-label
  $lambda78
  (lambda ($env nums k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda77) (make-env $env_t59 (nums nums)))
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
  (lambda ($env rv24)
    (if rv24
      (app*
       for-dict-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k19))
      (app* (env-ref $env_t40 $env k19) (void)))))
 (define-label
  $lambda43
  (lambda ($env rv23)
    (if rv23
      (app*
       for-py-list-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k19))
      ((cps dict?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda42)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k19 (env-ref $env_t40 $env k19))))))))
 (define-label
  $lambda44
  (lambda ($env rv22)
    (if rv22
      (app*
       for-tuple-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k19))
      ((cps py-list?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda43)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k19 (env-ref $env_t40 $env k19))))))))
 (define-label
  $lambda45
  (lambda ($env rv21)
    (if rv21
      (app*
       for-set-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k19))
      ((cps tuple?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda44)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k19 (env-ref $env_t40 $env k19))))))))
 (define-label
  $lambda46
  (lambda ($env k19)
    ((cps set?)
     (env-ref $env_t41 $env $seq14)
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t40
       ($loop15 (env-ref $env_t41 $env $loop15))
       ($seq14 (env-ref $env_t41 $env $seq14))
       (k19 k19))))))
 (define-label
  $lambda47
  (lambda ($env rv20) (app* (env-ref $env_t42 $env k18) (void))))
 (define-label
  $lambda48
  (lambda ($env $seq14 $loop15 k18)
    (app*
     (make-closure
      (lambda-label $lambda46)
      (make-env $env_t41 ($loop15 $loop15) ($seq14 $seq14)))
     (make-closure (lambda-label $lambda47) (make-env $env_t42 (k18 k18))))))
 (define-label
  $lambda49
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda50
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda49) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda51
  (lambda ($env rv38)
    (if rv38
      ((cps dict-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k35))
      (error "cannot index object" (env-ref $env_t43 $env k35)))))
 (define-label
  $lambda52
  (lambda ($env rv37)
    (if rv37
      ((cps tuple-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k35))
      ((cps dict?)
       (env-ref $env_t43 $env e16)
       (make-closure
        (lambda-label $lambda51)
        (make-env
         $env_t43
         (e16 (env-ref $env_t43 $env e16))
         (i15 (env-ref $env_t43 $env i15))
         (k35 (env-ref $env_t43 $env k35))))))))
 (define-label
  $lambda53
  (lambda ($env rv36)
    (if rv36
      ((cps py-list-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k35))
      ((cps tuple?)
       (env-ref $env_t43 $env e16)
       (make-closure
        (lambda-label $lambda52)
        (make-env
         $env_t43
         (e16 (env-ref $env_t43 $env e16))
         (i15 (env-ref $env_t43 $env i15))
         (k35 (env-ref $env_t43 $env k35))))))))
 (define-label
  $lambda54
  (lambda ($env i15 k35)
    ((cps py-list?)
     (env-ref $env_t44 $env e16)
     (make-closure
      (lambda-label $lambda53)
      (make-env
       $env_t43
       (e16 (env-ref $env_t44 $env e16))
       (i15 i15)
       (k35 k35))))))
 (define-label
  $lambda55
  (lambda ($env e16 k34)
    (app*
     (make-closure (lambda-label $lambda54) (make-env $env_t44 (e16 e16)))
     (get-cell (env-ref $env_t45 $env i))
     k34)))
 (define-label
  $lambda56
  (lambda ($env rv45)
    (if rv45
      ((cps dict-ref)
       (env-ref $env_t46 $env e18)
       (env-ref $env_t46 $env i17)
       (env-ref $env_t46 $env k42))
      (error "cannot index object" (env-ref $env_t46 $env k42)))))
 (define-label
  $lambda57
  (lambda ($env rv44)
    (if rv44
      ((cps tuple-ref)
       (env-ref $env_t46 $env e18)
       (env-ref $env_t46 $env i17)
       (env-ref $env_t46 $env k42))
      ((cps dict?)
       (env-ref $env_t46 $env e18)
       (make-closure
        (lambda-label $lambda56)
        (make-env
         $env_t46
         (e18 (env-ref $env_t46 $env e18))
         (i17 (env-ref $env_t46 $env i17))
         (k42 (env-ref $env_t46 $env k42))))))))
 (define-label
  $lambda58
  (lambda ($env rv43)
    (if rv43
      ((cps py-list-ref)
       (env-ref $env_t46 $env e18)
       (env-ref $env_t46 $env i17)
       (env-ref $env_t46 $env k42))
      ((cps tuple?)
       (env-ref $env_t46 $env e18)
       (make-closure
        (lambda-label $lambda57)
        (make-env
         $env_t46
         (e18 (env-ref $env_t46 $env e18))
         (i17 (env-ref $env_t46 $env i17))
         (k42 (env-ref $env_t46 $env k42))))))))
 (define-label
  $lambda59
  (lambda ($env i17 k42)
    ((cps py-list?)
     (env-ref $env_t47 $env e18)
     (make-closure
      (lambda-label $lambda58)
      (make-env
       $env_t46
       (e18 (env-ref $env_t47 $env e18))
       (i17 i17)
       (k42 k42))))))
 (define-env $env_t52 (return))
 (define-env $env_t53 (k30 return))
 (define-env $env_t54 (i nums return))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t56 (i k17 nums return))
 (define-env $env_t57 (k16 return))
 (define-env $env_t51 (i nums))
 (define-env $env_t59 (nums))
 (define-env $env_t43 (e16 i15 k35))
 (define-env $env_t44 (e16))
 (define-env $env_t45 (i))
 (define-env $env_t55 (i i14 nums return))
 (define-env $env_t58 (nums return))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t46 (e18 i17 k42))
 (define-env $env_t47 (e18))
 (define-env $env_t48 (e18 k41))
 (define-env $env_t40 ($loop15 $seq14 k19))
 (define-env $env_t49 (k31))
 (define-env $env_t50 (i k31 nums))
 (define-env $env_t42 (k18))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$pair_13 (void))
 (set-then!
  g$pair_13
  (make-closure (lambda-label $lambda78) (make-env $env_t39))
  (app* $halt (void))))
