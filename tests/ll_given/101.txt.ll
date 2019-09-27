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
  (lambda ($env rv19)
    (app* (env-ref $env_t40 $env return) rv19 (env-ref $env_t40 $env k18))))
 (define-label
  $lambda41
  (lambda ($env k18)
    ((cps -)
     (env-ref $env_t41 $env x)
     (make-closure
      (lambda-label $lambda40)
      (make-env $env_t40 (k18 k18) (return (env-ref $env_t41 $env return)))))))
 (define-label
  $lambda42
  (lambda ($env k20)
    (app* (env-ref $env_t41 $env return) (env-ref $env_t41 $env x) k20)))
 (define-label
  $lambda43
  (lambda ($env rv17)
    (if rv17
      (app*
       (make-closure
        (lambda-label $lambda41)
        (make-env
         $env_t41
         (return (env-ref $env_t42 $env return))
         (x (env-ref $env_t42 $env x))))
       (env-ref $env_t42 $env k16))
      (app*
       (make-closure
        (lambda-label $lambda42)
        (make-env
         $env_t41
         (return (env-ref $env_t42 $env return))
         (x (env-ref $env_t42 $env x))))
       (env-ref $env_t42 $env k16)))))
 (define-label
  $lambda44
  (lambda ($env k16)
    ((cps <)
     (env-ref $env_t41 $env x)
     0
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t42
       (k16 k16)
       (return (env-ref $env_t41 $env return))
       (x (env-ref $env_t41 $env x)))))))
 (define-label
  $lambda45
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda44)
      (make-env $env_t41 (return return) (x (env-ref $env_t43 $env x))))
     k15)))
 (define-label
  $lambda46
  (lambda ($env x k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda45) (make-env $env_t43 (x x)))
     k14)))
 (define-label $lambda47 (lambda ($env rv32) ((cps py-print) rv32 $halt)))
 (define-label
  $lambda48
  (lambda ($env rv31)
    (app*
     g$abs
     rv31
     (make-closure (lambda-label $lambda47) (make-env $env_t39)))))
 (define-label
  $lambda49
  (lambda ($env rv30)
    ((cps *)
     (env-ref $env_t44 $env rv29)
     rv30
     (make-closure (lambda-label $lambda48) (make-env $env_t39)))))
 (define-label
  $lambda50
  (lambda ($env rv29)
    ((cps -)
     6
     (make-closure (lambda-label $lambda49) (make-env $env_t44 (rv29 rv29))))))
 (define-label
  $lambda51
  (lambda ($env rv28)
    ((cps *)
     (env-ref $env_t45 $env rv27)
     rv28
     (make-closure (lambda-label $lambda50) (make-env $env_t39)))))
 (define-label
  $lambda52
  (lambda ($env rv27)
    ((cps -)
     5
     (make-closure (lambda-label $lambda51) (make-env $env_t45 (rv27 rv27))))))
 (define-label
  $lambda53
  (lambda ($env rv26)
    ((cps *)
     (env-ref $env_t46 $env rv25)
     rv26
     (make-closure (lambda-label $lambda52) (make-env $env_t39)))))
 (define-label
  $lambda54
  (lambda ($env rv25)
    ((cps -)
     4
     (make-closure (lambda-label $lambda53) (make-env $env_t46 (rv25 rv25))))))
 (define-label
  $lambda55
  (lambda ($env rv24)
    ((cps *)
     (env-ref $env_t47 $env rv23)
     rv24
     (make-closure (lambda-label $lambda54) (make-env $env_t39)))))
 (define-label
  $lambda56
  (lambda ($env rv23)
    ((cps -)
     3
     (make-closure (lambda-label $lambda55) (make-env $env_t47 (rv23 rv23))))))
 (define-label
  $lambda57
  (lambda ($env rv22)
    ((cps *)
     (env-ref $env_t48 $env rv21)
     rv22
     (make-closure (lambda-label $lambda56) (make-env $env_t39)))))
 (define-label
  $lambda58
  (lambda ($env rv21)
    ((cps -)
     2
     (make-closure (lambda-label $lambda57) (make-env $env_t48 (rv21 rv21))))))
 (define-env $env_t48 (rv21))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t47 (rv23))
 (define-env $env_t46 (rv25))
 (define-env $env_t44 (rv29))
 (define-env $env_t40 (k18 return))
 (define-env $env_t41 (return x))
 (define-env $env_t45 (rv27))
 (define-env $env_t42 (k16 return x))
 (define-env $env_t43 (x))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$abs (void))
 (set-then!
  g$abs
  (make-closure (lambda-label $lambda46) (make-env $env_t39))
  ((cps -) 1 (make-closure (lambda-label $lambda58) (make-env $env_t39)))))
