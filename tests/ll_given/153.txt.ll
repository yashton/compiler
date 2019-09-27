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
  (lambda ($env rv33)
    (if rv33
      ((cps dict-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k30))
      (error "cannot index object" (env-ref $env_t43 $env k30)))))
 (define-label
  $lambda50
  (lambda ($env rv32)
    (if rv32
      ((cps tuple-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k30))
      ((cps dict?)
       (env-ref $env_t43 $env e16)
       (make-closure
        (lambda-label $lambda49)
        (make-env
         $env_t43
         (e16 (env-ref $env_t43 $env e16))
         (i15 (env-ref $env_t43 $env i15))
         (k30 (env-ref $env_t43 $env k30))))))))
 (define-label
  $lambda51
  (lambda ($env rv31)
    (if rv31
      ((cps py-list-ref)
       (env-ref $env_t43 $env e16)
       (env-ref $env_t43 $env i15)
       (env-ref $env_t43 $env k30))
      ((cps tuple?)
       (env-ref $env_t43 $env e16)
       (make-closure
        (lambda-label $lambda50)
        (make-env
         $env_t43
         (e16 (env-ref $env_t43 $env e16))
         (i15 (env-ref $env_t43 $env i15))
         (k30 (env-ref $env_t43 $env k30))))))))
 (define-label
  $lambda52
  (lambda ($env i15 k30)
    ((cps py-list?)
     (env-ref $env_t44 $env e16)
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t43
       (e16 (env-ref $env_t44 $env e16))
       (i15 i15)
       (k30 k30))))))
 (define-label
  $lambda53
  (lambda ($env e16 k29)
    (app*
     (make-closure (lambda-label $lambda52) (make-env $env_t44 (e16 e16)))
     g$t
     k29)))
 (define-label
  $lambda54
  (lambda ($env rv34) ((cps py-print) "=>" rv34 (env-ref $env_t45 $env k28))))
 (define-label
  $lambda55
  (lambda ($env k28)
    (app*
     (make-closure (lambda-label $lambda53) (make-env $env_t39))
     g$fred
     (make-closure (lambda-label $lambda54) (make-env $env_t45 (k28 k28))))))
 (define-label
  $lambda56
  (lambda ($env k35) ((cps py-print) "is not present." k35)))
 (define-label
  $lambda57
  (lambda ($env rv27)
    (if rv27
      (app*
       (make-closure (lambda-label $lambda55) (make-env $env_t39))
       (env-ref $env_t46 $env k24))
      (app*
       (make-closure (lambda-label $lambda56) (make-env $env_t39))
       (env-ref $env_t46 $env k24)))))
 (define-label
  $lambda58
  (lambda ($env rv26)
    (app*
     rv26
     g$t
     (make-closure
      (lambda-label $lambda57)
      (make-env $env_t46 (k24 (env-ref $env_t46 $env k24)))))))
 (define-label
  $lambda59
  (lambda ($env rv25)
    (app*
     get-field
     g$fred
     has_key
     (make-closure
      (lambda-label $lambda58)
      (make-env $env_t46 (k24 (env-ref $env_t46 $env k24)))))))
 (define-label
  $lambda60
  (lambda ($env k24)
    ((cps py-print)
     g$t
     (make-closure (lambda-label $lambda59) (make-env $env_t46 (k24 k24))))))
 (define-label
  $lambda61
  (lambda ($env continue k23)
    (set-then!
     g$t
     (env-ref $env_t47 $env i14)
     (app* (make-closure (lambda-label $lambda60) (make-env $env_t39)) k23))))
 (define-label
  $lambda62
  (lambda ($env i14 k22)
    (app*
     (make-closure (lambda-label $lambda48) (make-env $env_t39))
     (make-closure (lambda-label $lambda61) (make-env $env_t47 (i14 i14)))
     k22)))
 (define-label
  $lambda63
  (lambda ($env break k14)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t39))
     g$test
     (make-closure (lambda-label $lambda62) (make-env $env_t39))
     k14)))
 (define-env $env_t45 (k28))
 (define-env $env_t40 ($loop15 $seq14 k16))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t39 ())
 (define-env $env_t42 (k15))
 (define-env $env_t43 (e16 i15 k30))
 (define-env $env_t44 (e16))
 (define-env $env_t38 (cc))
 (define-env $env_t46 (k24))
 (define-env $env_t47 (i14))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fred (void))
 (define g$test (void))
 (define g$t (void))
 (set-then!
  g$fred
  (dict ("mike" 456) ("bill" 399) ("sarah" 521))
  (set-then!
   g$test
   (py-list* "zingo" "sarah" "bill" "wilma")
   (app*
    (make-closure (lambda-label $lambda39) (make-env $env_t39))
    (make-closure (lambda-label $lambda63) (make-env $env_t39))
    $halt))))
