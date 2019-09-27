(program
 (define-label
  $lambda38
  (lambda ($env rv38)
    (if rv38
      ((cps dict-ref)
       (env-ref $env_t38 $env e15)
       (env-ref $env_t38 $env i14)
       (env-ref $env_t38 $env k35))
      (error "cannot index object" (env-ref $env_t38 $env k35)))))
 (define-label
  $lambda39
  (lambda ($env rv37)
    (if rv37
      ((cps tuple-ref)
       (env-ref $env_t38 $env e15)
       (env-ref $env_t38 $env i14)
       (env-ref $env_t38 $env k35))
      ((cps dict?)
       (env-ref $env_t38 $env e15)
       (make-closure
        (lambda-label $lambda38)
        (make-env
         $env_t38
         (e15 (env-ref $env_t38 $env e15))
         (i14 (env-ref $env_t38 $env i14))
         (k35 (env-ref $env_t38 $env k35))))))))
 (define-label
  $lambda40
  (lambda ($env rv36)
    (if rv36
      ((cps py-list-ref)
       (env-ref $env_t38 $env e15)
       (env-ref $env_t38 $env i14)
       (env-ref $env_t38 $env k35))
      ((cps tuple?)
       (env-ref $env_t38 $env e15)
       (make-closure
        (lambda-label $lambda39)
        (make-env
         $env_t38
         (e15 (env-ref $env_t38 $env e15))
         (i14 (env-ref $env_t38 $env i14))
         (k35 (env-ref $env_t38 $env k35))))))))
 (define-label
  $lambda41
  (lambda ($env i14 k35)
    ((cps py-list?)
     (env-ref $env_t39 $env e15)
     (make-closure
      (lambda-label $lambda40)
      (make-env
       $env_t38
       (e15 (env-ref $env_t39 $env e15))
       (i14 i14)
       (k35 k35))))))
 (define-label
  $lambda42
  (lambda ($env e15 k34)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39 (e15 e15)))
     1
     k34)))
 (define-label
  $lambda43
  (lambda ($env rv33)
    (if rv33
      ((cps dict-ref)
       (env-ref $env_t41 $env e17)
       (env-ref $env_t41 $env i16)
       (env-ref $env_t41 $env k30))
      (error "cannot index object" (env-ref $env_t41 $env k30)))))
 (define-label
  $lambda44
  (lambda ($env rv32)
    (if rv32
      ((cps tuple-ref)
       (env-ref $env_t41 $env e17)
       (env-ref $env_t41 $env i16)
       (env-ref $env_t41 $env k30))
      ((cps dict?)
       (env-ref $env_t41 $env e17)
       (make-closure
        (lambda-label $lambda43)
        (make-env
         $env_t41
         (e17 (env-ref $env_t41 $env e17))
         (i16 (env-ref $env_t41 $env i16))
         (k30 (env-ref $env_t41 $env k30))))))))
 (define-label
  $lambda45
  (lambda ($env rv31)
    (if rv31
      ((cps py-list-ref)
       (env-ref $env_t41 $env e17)
       (env-ref $env_t41 $env i16)
       (env-ref $env_t41 $env k30))
      ((cps tuple?)
       (env-ref $env_t41 $env e17)
       (make-closure
        (lambda-label $lambda44)
        (make-env
         $env_t41
         (e17 (env-ref $env_t41 $env e17))
         (i16 (env-ref $env_t41 $env i16))
         (k30 (env-ref $env_t41 $env k30))))))))
 (define-label
  $lambda46
  (lambda ($env i16 k30)
    ((cps py-list?)
     (env-ref $env_t42 $env e17)
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t41
       (e17 (env-ref $env_t42 $env e17))
       (i16 i16)
       (k30 k30))))))
 (define-label
  $lambda47
  (lambda ($env e17 k29)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t42 (e17 e17)))
     2
     k29)))
 (define-label
  $lambda48
  (lambda ($env rv28)
    (if rv28
      ((cps dict-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k25))
      (error "cannot index object" (env-ref $env_t43 $env k25)))))
 (define-label
  $lambda49
  (lambda ($env rv27)
    (if rv27
      ((cps tuple-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k25))
      ((cps dict?)
       (env-ref $env_t43 $env e19)
       (make-closure
        (lambda-label $lambda48)
        (make-env
         $env_t43
         (e19 (env-ref $env_t43 $env e19))
         (i18 (env-ref $env_t43 $env i18))
         (k25 (env-ref $env_t43 $env k25))))))))
 (define-label
  $lambda50
  (lambda ($env rv26)
    (if rv26
      ((cps py-list-ref)
       (env-ref $env_t43 $env e19)
       (env-ref $env_t43 $env i18)
       (env-ref $env_t43 $env k25))
      ((cps tuple?)
       (env-ref $env_t43 $env e19)
       (make-closure
        (lambda-label $lambda49)
        (make-env
         $env_t43
         (e19 (env-ref $env_t43 $env e19))
         (i18 (env-ref $env_t43 $env i18))
         (k25 (env-ref $env_t43 $env k25))))))))
 (define-label
  $lambda51
  (lambda ($env i18 k25)
    ((cps py-list?)
     (env-ref $env_t44 $env e19)
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t43
       (e19 (env-ref $env_t44 $env e19))
       (i18 i18)
       (k25 k25))))))
 (define-label
  $lambda52
  (lambda ($env e19 k24)
    (app*
     (make-closure (lambda-label $lambda51) (make-env $env_t44 (e19 e19)))
     3
     k24)))
 (define-label
  $lambda53
  (lambda ($env rv23)
    (if rv23
      ((cps dict-ref)
       (env-ref $env_t45 $env e21)
       (env-ref $env_t45 $env i20)
       (env-ref $env_t45 $env k20))
      (error "cannot index object" (env-ref $env_t45 $env k20)))))
 (define-label
  $lambda54
  (lambda ($env rv22)
    (if rv22
      ((cps tuple-ref)
       (env-ref $env_t45 $env e21)
       (env-ref $env_t45 $env i20)
       (env-ref $env_t45 $env k20))
      ((cps dict?)
       (env-ref $env_t45 $env e21)
       (make-closure
        (lambda-label $lambda53)
        (make-env
         $env_t45
         (e21 (env-ref $env_t45 $env e21))
         (i20 (env-ref $env_t45 $env i20))
         (k20 (env-ref $env_t45 $env k20))))))))
 (define-label
  $lambda55
  (lambda ($env rv21)
    (if rv21
      ((cps py-list-ref)
       (env-ref $env_t45 $env e21)
       (env-ref $env_t45 $env i20)
       (env-ref $env_t45 $env k20))
      ((cps tuple?)
       (env-ref $env_t45 $env e21)
       (make-closure
        (lambda-label $lambda54)
        (make-env
         $env_t45
         (e21 (env-ref $env_t45 $env e21))
         (i20 (env-ref $env_t45 $env i20))
         (k20 (env-ref $env_t45 $env k20))))))))
 (define-label
  $lambda56
  (lambda ($env i20 k20)
    ((cps py-list?)
     (env-ref $env_t46 $env e21)
     (make-closure
      (lambda-label $lambda55)
      (make-env
       $env_t45
       (e21 (env-ref $env_t46 $env e21))
       (i20 i20)
       (k20 k20))))))
 (define-label
  $lambda57
  (lambda ($env e21 k19)
    (app*
     (make-closure (lambda-label $lambda56) (make-env $env_t46 (e21 e21)))
     4
     k19)))
 (define-label
  $lambda58
  (lambda ($env rv18)
    (if rv18
      ((cps dict-ref)
       (env-ref $env_t47 $env e23)
       (env-ref $env_t47 $env i22)
       (env-ref $env_t47 $env k15))
      (error "cannot index object" (env-ref $env_t47 $env k15)))))
 (define-label
  $lambda59
  (lambda ($env rv17)
    (if rv17
      ((cps tuple-ref)
       (env-ref $env_t47 $env e23)
       (env-ref $env_t47 $env i22)
       (env-ref $env_t47 $env k15))
      ((cps dict?)
       (env-ref $env_t47 $env e23)
       (make-closure
        (lambda-label $lambda58)
        (make-env
         $env_t47
         (e23 (env-ref $env_t47 $env e23))
         (i22 (env-ref $env_t47 $env i22))
         (k15 (env-ref $env_t47 $env k15))))))))
 (define-label
  $lambda60
  (lambda ($env rv16)
    (if rv16
      ((cps py-list-ref)
       (env-ref $env_t47 $env e23)
       (env-ref $env_t47 $env i22)
       (env-ref $env_t47 $env k15))
      ((cps tuple?)
       (env-ref $env_t47 $env e23)
       (make-closure
        (lambda-label $lambda59)
        (make-env
         $env_t47
         (e23 (env-ref $env_t47 $env e23))
         (i22 (env-ref $env_t47 $env i22))
         (k15 (env-ref $env_t47 $env k15))))))))
 (define-label
  $lambda61
  (lambda ($env i22 k15)
    ((cps py-list?)
     (env-ref $env_t48 $env e23)
     (make-closure
      (lambda-label $lambda60)
      (make-env
       $env_t47
       (e23 (env-ref $env_t48 $env e23))
       (i22 i22)
       (k15 k15))))))
 (define-label
  $lambda62
  (lambda ($env e23 k14)
    (app*
     (make-closure (lambda-label $lambda61) (make-env $env_t48 (e23 e23)))
     5
     k14)))
 (define-label
  $lambda63
  (lambda ($env rv42)
    (app*
     (make-closure (lambda-label $lambda62) (make-env $env_t40))
     rv42
     $halt)))
 (define-label
  $lambda64
  (lambda ($env rv41)
    (app*
     (make-closure (lambda-label $lambda57) (make-env $env_t40))
     rv41
     (make-closure (lambda-label $lambda63) (make-env $env_t40)))))
 (define-label
  $lambda65
  (lambda ($env rv40)
    (app*
     (make-closure (lambda-label $lambda52) (make-env $env_t40))
     rv40
     (make-closure (lambda-label $lambda64) (make-env $env_t40)))))
 (define-label
  $lambda66
  (lambda ($env rv39)
    (app*
     (make-closure (lambda-label $lambda47) (make-env $env_t40))
     rv39
     (make-closure (lambda-label $lambda65) (make-env $env_t40)))))
 (define-env $env_t47 (e23 i22 k15))
 (define-env $env_t46 (e21))
 (define-env $env_t41 (e17 i16 k30))
 (define-env $env_t42 (e17))
 (define-env $env_t38 (e15 i14 k35))
 (define-env $env_t39 (e15))
 (define-env $env_t40 ())
 (define-env $env_t45 (e21 i20 k20))
 (define-env $env_t48 (e23))
 (define-env $env_t43 (e19 i18 k25))
 (define-env $env_t44 (e19))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (app*
  (make-closure (lambda-label $lambda42) (make-env $env_t40))
  g$a
  (make-closure (lambda-label $lambda66) (make-env $env_t40))))
