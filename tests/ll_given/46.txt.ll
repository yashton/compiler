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
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda43
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda42) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda44
  (lambda ($env rv30)
    (set-cell!
     (env-ref $env_t40 $env i)
     rv30
     (app* (env-ref $env_t40 $env k22) (void)))))
 (define-label
  $lambda45
  (lambda ($env rv29)
    (set-cell!
     (env-ref $env_t41 $env result)
     rv29
     ((cps +)
      (get-cell (env-ref $env_t41 $env i))
      1
      (make-closure
       (lambda-label $lambda44)
       (make-env
        $env_t40
        (i (env-ref $env_t41 $env i))
        (k22 (env-ref $env_t41 $env k22))))))))
 (define-label
  $lambda46
  (lambda ($env rv28)
    ((cps +)
     (get-cell (env-ref $env_t41 $env result))
     rv28
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t41
       (i (env-ref $env_t41 $env i))
       (k22 (env-ref $env_t41 $env k22))
       (result (env-ref $env_t41 $env result)))))))
 (define-label
  $lambda47
  (lambda ($env rv27)
    ((cps -)
     (env-ref $env_t42 $env rv23)
     rv27
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t41
       (i (env-ref $env_t42 $env i))
       (k22 (env-ref $env_t42 $env k22))
       (result (env-ref $env_t42 $env result)))))))
 (define-label
  $lambda48
  (lambda ($env rv26)
    (app*
     (env-ref $env_t43 $env rv24)
     rv26
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t42
       (i (env-ref $env_t43 $env i))
       (k22 (env-ref $env_t43 $env k22))
       (result (env-ref $env_t43 $env result))
       (rv23 (env-ref $env_t43 $env rv23)))))))
 (define-label
  $lambda49
  (lambda ($env rv25)
    ((cps /)
     rv25
     (get-cell (env-ref $env_t43 $env i))
     (make-closure
      (lambda-label $lambda48)
      (make-env
       $env_t43
       (i (env-ref $env_t43 $env i))
       (k22 (env-ref $env_t43 $env k22))
       (result (env-ref $env_t43 $env result))
       (rv23 (env-ref $env_t43 $env rv23))
       (rv24 (env-ref $env_t43 $env rv24)))))))
 (define-label
  $lambda50
  (lambda ($env rv24)
    ((cps +)
     (get-cell (env-ref $env_t42 $env i))
     1.0
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t43
       (i (env-ref $env_t42 $env i))
       (k22 (env-ref $env_t42 $env k22))
       (result (env-ref $env_t42 $env result))
       (rv23 (env-ref $env_t42 $env rv23))
       (rv24 rv24))))))
 (define-label
  $lambda51
  (lambda ($env rv23)
    (app*
     get-field
     g$math
     log
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t42
       (i (env-ref $env_t41 $env i))
       (k22 (env-ref $env_t41 $env k22))
       (result (env-ref $env_t41 $env result))
       (rv23 rv23))))))
 (define-label
  $lambda52
  (lambda ($env k22)
    ((cps /)
     1.0
     (get-cell (env-ref $env_t44 $env i))
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t41
       (i (env-ref $env_t44 $env i))
       (k22 k22)
       (result (env-ref $env_t44 $env result)))))))
 (define-label
  $lambda53
  (lambda ($env continue k21)
    (app*
     (make-closure
      (lambda-label $lambda52)
      (make-env
       $env_t44
       (i (env-ref $env_t44 $env i))
       (result (env-ref $env_t44 $env result))))
     k21)))
 (define-label
  $lambda54
  (lambda ($env rv31)
    (app*
     (get-cell (env-ref $env_t45 $env loop))
     (env-ref $env_t45 $env k19))))
 (define-label
  $lambda55
  (lambda ($env rv20)
    (if rv20
      (app*
       (make-closure (lambda-label $lambda43) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda53)
        (make-env
         $env_t44
         (i (env-ref $env_t46 $env i))
         (result (env-ref $env_t46 $env result))))
       (make-closure
        (lambda-label $lambda54)
        (make-env
         $env_t45
         (k19 (env-ref $env_t46 $env k19))
         (loop (env-ref $env_t46 $env loop)))))
      (app* (env-ref $env_t46 $env k19) (void)))))
 (define-label
  $lambda56
  (lambda ($env k19)
    ((cps <=)
     (get-cell (env-ref $env_t47 $env i))
     500000
     (make-closure
      (lambda-label $lambda55)
      (make-env
       $env_t46
       (i (env-ref $env_t47 $env i))
       (k19 k19)
       (loop (env-ref $env_t47 $env loop))
       (result (env-ref $env_t47 $env result)))))))
 (define-label
  $lambda57
  (lambda ($env rv32) (app* (env-ref $env_t48 $env k18) (void))))
 (define-label
  $lambda58
  (lambda ($env loop k18)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure
       (lambda-label $lambda56)
       (make-env
        $env_t47
        (i (env-ref $env_t44 $env i))
        (loop loop)
        (result (env-ref $env_t44 $env result))))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda57)
        (make-env $env_t48 (k18 k18))))))))
 (define-label
  $lambda59
  (lambda ($env break k17)
    (app*
     (make-closure
      (lambda-label $lambda58)
      (make-env
       $env_t44
       (i (env-ref $env_t44 $env i))
       (result (env-ref $env_t44 $env result))))
     (void)
     k17)))
 (define-label
  $lambda60
  (lambda ($env rv33)
    (app*
     (env-ref $env_t49 $env return)
     (get-cell (env-ref $env_t49 $env result))
     (env-ref $env_t49 $env k16))))
 (define-label
  $lambda61
  (lambda ($env result i k16)
    (set-then!
     result
     (make-cell result)
     (set-then!
      i
      (make-cell i)
      (set-cell!
       result
       0.0
       (set-cell!
        i
        1
        (app*
         (make-closure (lambda-label $lambda41) (make-env $env_t39))
         (make-closure
          (lambda-label $lambda59)
          (make-env $env_t44 (i i) (result result)))
         (make-closure
          (lambda-label $lambda60)
          (make-env
           $env_t49
           (k16 k16)
           (result result)
           (return (env-ref $env_t50 $env return)))))))))))
 (define-label
  $lambda62
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda61)
      (make-env $env_t50 (return return)))
     (void)
     (void)
     k15)))
 (define-label
  $lambda63
  (lambda ($env k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda62) (make-env $env_t39))
     k14)))
 (define-env $env_t45 (k19 loop))
 (define-env $env_t46 (i k19 loop result))
 (define-env $env_t47 (i loop result))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t49 (k16 result return))
 (define-env $env_t48 (k18))
 (define-env $env_t50 (return))
 (define-env $env_t40 (i k22))
 (define-env $env_t41 (i k22 result))
 (define-env $env_t42 (i k22 result rv23))
 (define-env $env_t43 (i k22 result rv23 rv24))
 (define-env $env_t44 (i result))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$gamma (void))
 (set-then!
  g$gamma
  (make-closure (lambda-label $lambda63) (make-env $env_t39))
  (app* $halt (void))))
