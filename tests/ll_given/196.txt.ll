(program
 (define-label
  $lambda38
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-label
  $lambda39
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda38) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label $lambda40 (lambda ($env k35) (app* k35 (void))))
 (define-label
  $lambda41
  (lambda ($env k17)
    (if #f
      (app* (make-closure (lambda-label $lambda40) (make-env $env_t39)) k17)
      (app* k17 (void)))))
 (define-label
  $lambda42
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-label
  $lambda43
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda42) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda44
  (lambda ($env rv26)
    (if rv26
      (app*
       for-dict-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k21))
      (app* (env-ref $env_t40 $env k21) (void)))))
 (define-label
  $lambda45
  (lambda ($env rv25)
    (if rv25
      (app*
       for-py-list-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k21))
      ((cps dict?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda44)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k21 (env-ref $env_t40 $env k21))))))))
 (define-label
  $lambda46
  (lambda ($env rv24)
    (if rv24
      (app*
       for-tuple-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k21))
      ((cps py-list?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda45)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k21 (env-ref $env_t40 $env k21))))))))
 (define-label
  $lambda47
  (lambda ($env rv23)
    (if rv23
      (app*
       for-set-k
       (env-ref $env_t40 $env $seq14)
       (env-ref $env_t40 $env $loop15)
       (env-ref $env_t40 $env k21))
      ((cps tuple?)
       (env-ref $env_t40 $env $seq14)
       (make-closure
        (lambda-label $lambda46)
        (make-env
         $env_t40
         ($loop15 (env-ref $env_t40 $env $loop15))
         ($seq14 (env-ref $env_t40 $env $seq14))
         (k21 (env-ref $env_t40 $env k21))))))))
 (define-label
  $lambda48
  (lambda ($env k21)
    ((cps set?)
     (env-ref $env_t41 $env $seq14)
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t40
       ($loop15 (env-ref $env_t41 $env $loop15))
       ($seq14 (env-ref $env_t41 $env $seq14))
       (k21 k21))))))
 (define-label
  $lambda49
  (lambda ($env rv22) (app* (env-ref $env_t42 $env k20) (void))))
 (define-label
  $lambda50
  (lambda ($env $seq14 $loop15 k20)
    (app*
     (make-closure
      (lambda-label $lambda48)
      (make-env $env_t41 ($loop15 $loop15) ($seq14 $seq14)))
     (make-closure (lambda-label $lambda49) (make-env $env_t42 (k20 k20))))))
 (define-label
  $lambda51
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-label
  $lambda52
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda51) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda53
  (lambda ($env continue k28)
    (set-then! g$g (env-ref $env_t43 $env i14) (app* k28 (void)))))
 (define-label
  $lambda54
  (lambda ($env i14 k27)
    (app*
     (make-closure (lambda-label $lambda52) (make-env $env_t39))
     (make-closure (lambda-label $lambda53) (make-env $env_t43 (i14 i14)))
     k27)))
 (define-label
  $lambda55
  (lambda ($env break k19)
    (app*
     (make-closure (lambda-label $lambda50) (make-env $env_t39))
     (py-list* 1 2 3)
     (make-closure (lambda-label $lambda54) (make-env $env_t39))
     k19)))
 (define-label
  $lambda56
  (lambda ($env x k)
    (set-then!
     x
     (make-cell x)
     (app* (env-ref $env_t38 $env cc) (get-cell x)))))
 (define-label
  $lambda57
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda56) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda58
  (lambda ($env g k32)
    (set-then!
     g
     (make-cell g)
     (set-cell!
      g
      (get-cell (env-ref $env_t44 $env x))
      ((cps py-print) (get-cell g) k32)))))
 (define-label
  $lambda59
  (lambda ($env return k31)
    (app*
     (make-closure
      (lambda-label $lambda58)
      (make-env $env_t44 (x (env-ref $env_t44 $env x))))
     (void)
     k31)))
 (define-label
  $lambda60
  (lambda ($env k30)
    (app*
     (make-closure (lambda-label $lambda57) (make-env $env_t39))
     (make-closure
      (lambda-label $lambda59)
      (make-env $env_t44 (x (env-ref $env_t44 $env x))))
     k30)))
 (define-label
  $lambda61
  (lambda ($env rv34)
    (app* (env-ref $env_t45 $env return) g$g (env-ref $env_t45 $env k16))))
 (define-label
  $lambda62
  (lambda ($env rv33)
    ((cps py-print)
     g$g
     (make-closure
      (lambda-label $lambda61)
      (make-env
       $env_t45
       (k16 (env-ref $env_t45 $env k16))
       (return (env-ref $env_t45 $env return)))))))
 (define-label
  $lambda63
  (lambda ($env rv29)
    (set-cell!
     (env-ref $env_t46 $env x)
     314
     (set-cell!
      (env-ref $env_t46 $env h)
      (make-closure
       (lambda-label $lambda60)
       (make-env $env_t44 (x (env-ref $env_t46 $env x))))
      (app*
       (get-cell (env-ref $env_t46 $env h))
       (make-closure
        (lambda-label $lambda62)
        (make-env
         $env_t45
         (k16 (env-ref $env_t46 $env k16))
         (return (env-ref $env_t46 $env return)))))))))
 (define-label
  $lambda64
  (lambda ($env rv18)
    (app*
     (make-closure (lambda-label $lambda43) (make-env $env_t39))
     (make-closure (lambda-label $lambda55) (make-env $env_t39))
     (make-closure
      (lambda-label $lambda63)
      (make-env
       $env_t46
       (h (env-ref $env_t46 $env h))
       (k16 (env-ref $env_t46 $env k16))
       (return (env-ref $env_t46 $env return))
       (x (env-ref $env_t46 $env x)))))))
 (define-label
  $lambda65
  (lambda ($env h x k16)
    (set-then!
     h
     (make-cell h)
     (set-then!
      x
      (make-cell x)
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda64)
        (make-env
         $env_t46
         (h h)
         (k16 k16)
         (return (env-ref $env_t47 $env return))
         (x x))))))))
 (define-label
  $lambda66
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda65)
      (make-env $env_t47 (return return)))
     (void)
     (void)
     k15)))
 (define-label
  $lambda67
  (lambda ($env k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda66) (make-env $env_t39))
     k14)))
 (define-label $lambda68 (lambda ($env rv37) ((cps py-print) g$g $halt)))
 (define-label
  $lambda69
  (lambda ($env rv36)
    ((cps py-print)
     rv36
     (make-closure (lambda-label $lambda68) (make-env $env_t39)))))
 (define-env $env_t43 (i14))
 (define-env $env_t47 (return))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t44 (x))
 (define-env $env_t40 ($loop15 $seq14 k21))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t46 (h k16 return x))
 (define-env $env_t42 (k20))
 (define-env $env_t45 (k16 return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$g (void))
 (set-then!
  g$g
  200
  (set-then!
   g$f
   (make-closure (lambda-label $lambda67) (make-env $env_t39))
   (app* g$f (make-closure (lambda-label $lambda69) (make-env $env_t39))))))
