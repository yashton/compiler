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
  (lambda ($env rv26)
    ((cps py-print) "Area of square = " rv26 (env-ref $env_t40 $env k23))))
 (define-label
  $lambda43
  (lambda ($env rv25)
    (set-then!
     g$length
     rv25
     ((cps expt)
      g$length
      2
      (make-closure
       (lambda-label $lambda42)
       (make-env $env_t40 (k23 (env-ref $env_t40 $env k23))))))))
 (define-label
  $lambda44
  (lambda ($env rv24)
    (app*
     g$float
     rv24
     (make-closure
      (lambda-label $lambda43)
      (make-env $env_t40 (k23 (env-ref $env_t40 $env k23)))))))
 (define-label
  $lambda45
  (lambda ($env k23)
    (app*
     g$input
     "Length: "
     (make-closure (lambda-label $lambda44) (make-env $env_t40 (k23 k23))))))
 (define-label
  $lambda46
  (lambda ($env rv33)
    ((cps py-print) "Area of rectangle = " rv33 (env-ref $env_t41 $env k28))))
 (define-label
  $lambda47
  (lambda ($env rv32)
    (set-then!
     g$width
     rv32
     ((cps *)
      g$length
      g$width
      (make-closure
       (lambda-label $lambda46)
       (make-env $env_t41 (k28 (env-ref $env_t41 $env k28))))))))
 (define-label
  $lambda48
  (lambda ($env rv31)
    (app*
     g$float
     rv31
     (make-closure
      (lambda-label $lambda47)
      (make-env $env_t41 (k28 (env-ref $env_t41 $env k28)))))))
 (define-label
  $lambda49
  (lambda ($env rv30)
    (set-then!
     g$length
     rv30
     (app*
      g$input
      "Width: "
      (make-closure
       (lambda-label $lambda48)
       (make-env $env_t41 (k28 (env-ref $env_t41 $env k28))))))))
 (define-label
  $lambda50
  (lambda ($env rv29)
    (app*
     g$float
     rv29
     (make-closure
      (lambda-label $lambda49)
      (make-env $env_t41 (k28 (env-ref $env_t41 $env k28)))))))
 (define-label
  $lambda51
  (lambda ($env k28)
    (app*
     g$input
     "Length: "
     (make-closure (lambda-label $lambda50) (make-env $env_t41 (k28 k28))))))
 (define-label
  $lambda52
  (lambda ($env k34) ((cps py-print) " Not a valid shape. try again" k34)))
 (define-label
  $lambda53
  (lambda ($env rv27)
    (if rv27
      (app*
       (make-closure (lambda-label $lambda51) (make-env $env_t39))
       (env-ref $env_t42 $env k21))
      (app*
       (make-closure (lambda-label $lambda52) (make-env $env_t39))
       (env-ref $env_t42 $env k21)))))
 (define-label
  $lambda54
  (lambda ($env rv22)
    (if rv22
      (app*
       (make-closure (lambda-label $lambda45) (make-env $env_t39))
       (env-ref $env_t42 $env k21))
      ((cps equal?)
       g$shape
       2
       (make-closure
        (lambda-label $lambda53)
        (make-env $env_t42 (k21 (env-ref $env_t42 $env k21))))))))
 (define-label
  $lambda55
  (lambda ($env k21)
    ((cps equal?)
     g$shape
     1
     (make-closure (lambda-label $lambda54) (make-env $env_t42 (k21 k21))))))
 (define-label
  $lambda56
  (lambda ($env continue k20)
    (app* (make-closure (lambda-label $lambda55) (make-env $env_t39)) k20)))
 (define-label
  $lambda57
  (lambda ($env rv35)
    (app*
     (get-cell (env-ref $env_t43 $env loop))
     (env-ref $env_t43 $env k18))))
 (define-label
  $lambda58
  (lambda ($env rv19)
    (if rv19
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure (lambda-label $lambda56) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda57)
        (make-env
         $env_t43
         (k18 (env-ref $env_t43 $env k18))
         (loop (env-ref $env_t43 $env loop)))))
      (app* (env-ref $env_t43 $env k18) (void)))))
 (define-label
  $lambda59
  (lambda ($env k18)
    ((cps not-equal?)
     g$shape
     4
     (make-closure
      (lambda-label $lambda58)
      (make-env $env_t43 (k18 k18) (loop (env-ref $env_t44 $env loop)))))))
 (define-label
  $lambda60
  (lambda ($env rv36) (app* (env-ref $env_t45 $env k17) (void))))
 (define-label
  $lambda61
  (lambda ($env loop k17)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure (lambda-label $lambda59) (make-env $env_t44 (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda60)
        (make-env $env_t45 (k17 k17))))))))
 (define-label
  $lambda62
  (lambda ($env break k16)
    (app*
     (make-closure (lambda-label $lambda61) (make-env $env_t39))
     (void)
     k16)))
 (define-label
  $lambda63
  (lambda ($env rv15)
    (set-then!
     g$shape
     rv15
     (app*
      (make-closure (lambda-label $lambda39) (make-env $env_t39))
      (make-closure (lambda-label $lambda62) (make-env $env_t39))
      $halt))))
 (define-label
  $lambda64
  (lambda ($env rv14)
    (app*
     g$int
     rv14
     (make-closure (lambda-label $lambda63) (make-env $env_t39)))))
 (define-env $env_t41 (k28))
 (define-env $env_t42 (k21))
 (define-env $env_t43 (k18 loop))
 (define-env $env_t44 (loop))
 (define-env $env_t45 (k17))
 (define-env $env_t40 (k23))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$length (void))
 (define g$shape (void))
 (define g$width (void))
 (app*
  g$input
  g$menu
  (make-closure (lambda-label $lambda64) (make-env $env_t39))))
