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
  (lambda ($env rv24)
    (set-cell!
     (env-ref $env_t40 $env i)
     rv24
     (app* (env-ref $env_t40 $env k22) (void)))))
 (define-label
  $lambda45
  (lambda ($env rv23)
    (set-cell!
     (env-ref $env_t41 $env result)
     rv23
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
  (lambda ($env k22)
    ((cps +)
     (get-cell (env-ref $env_t42 $env result))
     (get-cell (env-ref $env_t42 $env i))
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t41
       (i (env-ref $env_t42 $env i))
       (k22 k22)
       (result (env-ref $env_t42 $env result)))))))
 (define-label
  $lambda47
  (lambda ($env continue k21)
    (app*
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t42
       (i (env-ref $env_t42 $env i))
       (result (env-ref $env_t42 $env result))))
     k21)))
 (define-label
  $lambda48
  (lambda ($env rv25)
    (app*
     (get-cell (env-ref $env_t43 $env loop))
     (env-ref $env_t43 $env k19))))
 (define-label
  $lambda49
  (lambda ($env rv20)
    (if rv20
      (app*
       (make-closure (lambda-label $lambda43) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda47)
        (make-env
         $env_t42
         (i (env-ref $env_t44 $env i))
         (result (env-ref $env_t44 $env result))))
       (make-closure
        (lambda-label $lambda48)
        (make-env
         $env_t43
         (k19 (env-ref $env_t44 $env k19))
         (loop (env-ref $env_t44 $env loop)))))
      (app* (env-ref $env_t44 $env k19) (void)))))
 (define-label
  $lambda50
  (lambda ($env k19)
    ((cps <=)
     (get-cell (env-ref $env_t45 $env i))
     (env-ref $env_t45 $env n)
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t44
       (i (env-ref $env_t45 $env i))
       (k19 k19)
       (loop (env-ref $env_t45 $env loop))
       (result (env-ref $env_t45 $env result)))))))
 (define-label
  $lambda51
  (lambda ($env rv26) (app* (env-ref $env_t46 $env k18) (void))))
 (define-label
  $lambda52
  (lambda ($env loop k18)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure
       (lambda-label $lambda50)
       (make-env
        $env_t45
        (i (env-ref $env_t47 $env i))
        (loop loop)
        (n (env-ref $env_t47 $env n))
        (result (env-ref $env_t47 $env result))))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda51)
        (make-env $env_t46 (k18 k18))))))))
 (define-label
  $lambda53
  (lambda ($env break k17)
    (app*
     (make-closure
      (lambda-label $lambda52)
      (make-env
       $env_t47
       (i (env-ref $env_t47 $env i))
       (n (env-ref $env_t47 $env n))
       (result (env-ref $env_t47 $env result))))
     (void)
     k17)))
 (define-label
  $lambda54
  (lambda ($env rv27)
    (app*
     (env-ref $env_t48 $env return)
     (get-cell (env-ref $env_t48 $env result))
     (env-ref $env_t48 $env k16))))
 (define-label
  $lambda55
  (lambda ($env result i k16)
    (set-then!
     result
     (make-cell result)
     (set-then!
      i
      (make-cell i)
      (set-cell!
       result
       0
       (set-cell!
        i
        1
        (app*
         (make-closure (lambda-label $lambda41) (make-env $env_t39))
         (make-closure
          (lambda-label $lambda53)
          (make-env
           $env_t47
           (i i)
           (n (env-ref $env_t49 $env n))
           (result result)))
         (make-closure
          (lambda-label $lambda54)
          (make-env
           $env_t48
           (k16 k16)
           (result result)
           (return (env-ref $env_t49 $env return)))))))))))
 (define-label
  $lambda56
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda55)
      (make-env $env_t49 (n (env-ref $env_t50 $env n)) (return return)))
     (void)
     (void)
     k15)))
 (define-label
  $lambda57
  (lambda ($env n k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda56) (make-env $env_t50 (n n)))
     k14)))
 (define-env $env_t48 (k16 result return))
 (define-env $env_t49 (n return))
 (define-env $env_t50 (n))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (i k22))
 (define-env $env_t41 (i k22 result))
 (define-env $env_t42 (i result))
 (define-env $env_t43 (k19 loop))
 (define-env $env_t44 (i k19 loop result))
 (define-env $env_t45 (i loop n result))
 (define-env $env_t46 (k18))
 (define-env $env_t47 (i n result))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$sum (void))
 (set-then!
  g$sum
  (make-closure (lambda-label $lambda57) (make-env $env_t39))
  (app* $halt (void))))
