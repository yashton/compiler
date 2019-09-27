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
    (set-then! g$sum rv26 (app* (env-ref $env_t40 $env k25) (void)))))
 (define-label
  $lambda43
  (lambda ($env k25)
    ((cps +)
     g$sum
     g$x
     (make-closure (lambda-label $lambda42) (make-env $env_t40 (k25 k25))))))
 (define-label
  $lambda44
  (lambda ($env rv28)
    (set-then!
     g$x
     rv28
     (app* (env-ref $env_t41 $env continue) (env-ref $env_t41 $env k27)))))
 (define-label
  $lambda45
  (lambda ($env k27)
    ((cps +)
     g$x
     1
     (make-closure
      (lambda-label $lambda44)
      (make-env
       $env_t41
       (continue (env-ref $env_t42 $env continue))
       (k27 k27))))))
 (define-label
  $lambda46
  (lambda ($env rv24)
    (if rv24
      (app*
       (make-closure (lambda-label $lambda43) (make-env $env_t39))
       (env-ref $env_t43 $env k20))
      (app*
       (make-closure
        (lambda-label $lambda45)
        (make-env $env_t42 (continue (env-ref $env_t43 $env continue))))
       (env-ref $env_t43 $env k20)))))
 (define-label
  $lambda47
  (lambda ($env rv23)
    ((cps equal?)
     rv23
     0
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t43
       (continue (env-ref $env_t43 $env continue))
       (k20 (env-ref $env_t43 $env k20)))))))
 (define-label
  $lambda48
  (lambda ($env k20)
    ((cps modulo)
     g$x
     2
     (make-closure
      (lambda-label $lambda47)
      (make-env
       $env_t43
       (continue (env-ref $env_t42 $env continue))
       (k20 k20))))))
 (define-label
  $lambda49
  (lambda ($env rv22)
    (set-then! g$x rv22 (app* (env-ref $env_t44 $env k19) (void)))))
 (define-label
  $lambda50
  (lambda ($env rv21)
    ((cps +)
     g$x
     1
     (make-closure
      (lambda-label $lambda49)
      (make-env $env_t44 (k19 (env-ref $env_t44 $env k19)))))))
 (define-label
  $lambda51
  (lambda ($env k19)
    (app*
     (make-closure
      (lambda-label $lambda48)
      (make-env $env_t42 (continue (env-ref $env_t42 $env continue))))
     (make-closure (lambda-label $lambda50) (make-env $env_t44 (k19 k19))))))
 (define-label
  $lambda52
  (lambda ($env continue k18)
    (app*
     (make-closure
      (lambda-label $lambda51)
      (make-env $env_t42 (continue continue)))
     k18)))
 (define-label
  $lambda53
  (lambda ($env rv29)
    (app*
     (get-cell (env-ref $env_t45 $env loop))
     (env-ref $env_t45 $env k16))))
 (define-label
  $lambda54
  (lambda ($env rv17)
    (if rv17
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure (lambda-label $lambda52) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda53)
        (make-env
         $env_t45
         (k16 (env-ref $env_t45 $env k16))
         (loop (env-ref $env_t45 $env loop)))))
      (app* (env-ref $env_t45 $env k16) (void)))))
 (define-label
  $lambda55
  (lambda ($env k16)
    ((cps <=)
     g$x
     20
     (make-closure
      (lambda-label $lambda54)
      (make-env $env_t45 (k16 k16) (loop (env-ref $env_t46 $env loop)))))))
 (define-label
  $lambda56
  (lambda ($env k31) ((cps py-print) "didn't run\n" k31)))
 (define-label
  $lambda57
  (lambda ($env rv30)
    (app*
     (make-closure (lambda-label $lambda56) (make-env $env_t39))
     (env-ref $env_t47 $env k15))))
 (define-label
  $lambda58
  (lambda ($env loop k15)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure (lambda-label $lambda55) (make-env $env_t46 (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda57)
        (make-env $env_t47 (k15 k15))))))))
 (define-label
  $lambda59
  (lambda ($env break k14)
    (app*
     (make-closure (lambda-label $lambda58) (make-env $env_t39))
     (void)
     k14)))
 (define-label $lambda60 (lambda ($env rv32) ((cps py-print) g$sum $halt)))
 (define-env $env_t47 (k15))
 (define-env $env_t40 (k25))
 (define-env $env_t38 (cc))
 (define-env $env_t41 (continue k27))
 (define-env $env_t42 (continue))
 (define-env $env_t43 (continue k20))
 (define-env $env_t39 ())
 (define-env $env_t44 (k19))
 (define-env $env_t45 (k16 loop))
 (define-env $env_t46 (loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$sum (void))
 (set-then!
  g$x
  0
  (set-then!
   g$sum
   0
   (app*
    (make-closure (lambda-label $lambda39) (make-env $env_t39))
    (make-closure (lambda-label $lambda59) (make-env $env_t39))
    (make-closure (lambda-label $lambda60) (make-env $env_t39))))))
