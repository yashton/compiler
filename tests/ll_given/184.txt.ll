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
  (lambda ($env k27) (app* (env-ref $env_t40 $env break) k27)))
 (define-label
  $lambda43
  (lambda ($env rv26)
    (if rv26
      (app*
       (make-closure
        (lambda-label $lambda42)
        (make-env $env_t40 (break (env-ref $env_t41 $env break))))
       (env-ref $env_t41 $env k20))
      (app* (env-ref $env_t41 $env k20) (void)))))
 (define-label
  $lambda44
  (lambda ($env k20)
    ((cps >)
     g$count
     10
     (make-closure
      (lambda-label $lambda43)
      (make-env $env_t41 (break (env-ref $env_t40 $env break)) (k20 k20))))))
 (define-label
  $lambda45
  (lambda ($env k25) (app* (env-ref $env_t42 $env continue) k25)))
 (define-label
  $lambda46
  (lambda ($env rv24)
    (if rv24
      (app*
       (make-closure
        (lambda-label $lambda45)
        (make-env $env_t42 (continue (env-ref $env_t43 $env continue))))
       (env-ref $env_t43 $env k22))
      (app* (env-ref $env_t43 $env k22) (void)))))
 (define-label
  $lambda47
  (lambda ($env k22)
    ((cps equal?)
     g$count
     5
     (make-closure
      (lambda-label $lambda46)
      (make-env
       $env_t43
       (continue (env-ref $env_t42 $env continue))
       (k22 k22))))))
 (define-label
  $lambda48
  (lambda ($env rv23) ((cps py-print) g$count (env-ref $env_t44 $env k18))))
 (define-label
  $lambda49
  (lambda ($env rv21)
    (app*
     (make-closure
      (lambda-label $lambda47)
      (make-env $env_t42 (continue (env-ref $env_t45 $env continue))))
     (make-closure
      (lambda-label $lambda48)
      (make-env $env_t44 (k18 (env-ref $env_t45 $env k18)))))))
 (define-label
  $lambda50
  (lambda ($env rv19)
    (set-then!
     g$count
     rv19
     (app*
      (make-closure
       (lambda-label $lambda44)
       (make-env $env_t40 (break (env-ref $env_t46 $env break))))
      (make-closure
       (lambda-label $lambda49)
       (make-env
        $env_t45
        (continue (env-ref $env_t46 $env continue))
        (k18 (env-ref $env_t46 $env k18))))))))
 (define-label
  $lambda51
  (lambda ($env k18)
    ((cps +)
     g$count
     1
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t46
       (break (env-ref $env_t47 $env break))
       (continue (env-ref $env_t47 $env continue))
       (k18 k18))))))
 (define-label
  $lambda52
  (lambda ($env continue k17)
    (app*
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t47
       (break (env-ref $env_t40 $env break))
       (continue continue)))
     k17)))
 (define-label
  $lambda53
  (lambda ($env rv28)
    (app*
     (get-cell (env-ref $env_t48 $env loop))
     (env-ref $env_t48 $env k16))))
 (define-label
  $lambda54
  (lambda ($env k16)
    (if #t
      (app*
       (make-closure (lambda-label $lambda41) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda52)
        (make-env $env_t40 (break (env-ref $env_t49 $env break))))
       (make-closure
        (lambda-label $lambda53)
        (make-env $env_t48 (k16 k16) (loop (env-ref $env_t49 $env loop)))))
      (app* k16 (void)))))
 (define-label
  $lambda55
  (lambda ($env rv29) (app* (env-ref $env_t50 $env k15) (void))))
 (define-label
  $lambda56
  (lambda ($env loop k15)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure
       (lambda-label $lambda54)
       (make-env $env_t49 (break (env-ref $env_t40 $env break)) (loop loop)))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda55)
        (make-env $env_t50 (k15 k15))))))))
 (define-label
  $lambda57
  (lambda ($env break k14)
    (app*
     (make-closure (lambda-label $lambda56) (make-env $env_t40 (break break)))
     (void)
     k14)))
 (define-env $env_t50 (k15))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t40 (break))
 (define-env $env_t41 (break k20))
 (define-env $env_t42 (continue))
 (define-env $env_t43 (continue k22))
 (define-env $env_t44 (k18))
 (define-env $env_t45 (continue k18))
 (define-env $env_t46 (break continue k18))
 (define-env $env_t47 (break continue))
 (define-env $env_t48 (k16 loop))
 (define-env $env_t49 (break loop))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$count (void))
 (set-then!
  g$count
  0
  (app*
   (make-closure (lambda-label $lambda39) (make-env $env_t39))
   (make-closure (lambda-label $lambda57) (make-env $env_t39))
   $halt)))
