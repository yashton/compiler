(program
 (define-label
  $lambda47
  (lambda ($env b17 k20)
    (app*
     (make-closure (lambda-label $lambda46) (make-env $env_t42 (b17 b17)))
     2
     k20)))
 (define-label $lambda48 (lambda ($env rv26) ((cps py-print) g$b $halt)))
 (define-label
  $lambda49
  (lambda ($env rv25)
    ((cps py-print)
     g$a
     (make-closure (lambda-label $lambda48) (make-env $env_t40)))))
 (define-label
  $lambda50
  (lambda ($env rv19)
    (app*
     (make-closure (lambda-label $lambda47) (make-env $env_t40))
     g$b
     (make-closure (lambda-label $lambda49) (make-env $env_t40)))))
 (define-label
  $lambda38
  (lambda ($env rv18)
    (if rv18
      ((cps dict-remove!)
       (env-ref $env_t38 $env b15)
       (env-ref $env_t38 $env i14)
       (env-ref $env_t38 $env k15))
      (app* (env-ref $env_t38 $env k15) (void)))))
 (define-label
  $lambda39
  (lambda ($env rv17)
    (if rv17
      ((cps py-list-remove!)
       (env-ref $env_t38 $env b15)
       (env-ref $env_t38 $env i14)
       (env-ref $env_t38 $env k15))
      ((cps dict?)
       (env-ref $env_t38 $env b15)
       (make-closure
        (lambda-label $lambda38)
        (make-env
         $env_t38
         (b15 (env-ref $env_t38 $env b15))
         (i14 (env-ref $env_t38 $env i14))
         (k15 (env-ref $env_t38 $env k15))))))))
 (define-label
  $lambda40
  (lambda ($env rv16)
    (if rv16
      (error "Cannot delete from tuples!" (env-ref $env_t38 $env k15))
      ((cps py-list?)
       (env-ref $env_t38 $env b15)
       (make-closure
        (lambda-label $lambda39)
        (make-env
         $env_t38
         (b15 (env-ref $env_t38 $env b15))
         (i14 (env-ref $env_t38 $env i14))
         (k15 (env-ref $env_t38 $env k15))))))))
 (define-label
  $lambda41
  (lambda ($env i14 k15)
    ((cps tuple?)
     (env-ref $env_t39 $env b15)
     (make-closure
      (lambda-label $lambda40)
      (make-env
       $env_t38
       (b15 (env-ref $env_t39 $env b15))
       (i14 i14)
       (k15 k15))))))
 (define-label
  $lambda42
  (lambda ($env b15 k14)
    (app*
     (make-closure (lambda-label $lambda41) (make-env $env_t39 (b15 b15)))
     1
     k14)))
 (define-label
  $lambda43
  (lambda ($env rv24)
    (if rv24
      ((cps dict-remove!)
       (env-ref $env_t41 $env b17)
       (env-ref $env_t41 $env i16)
       (env-ref $env_t41 $env k21))
      (app* (env-ref $env_t41 $env k21) (void)))))
 (define-label
  $lambda44
  (lambda ($env rv23)
    (if rv23
      ((cps py-list-remove!)
       (env-ref $env_t41 $env b17)
       (env-ref $env_t41 $env i16)
       (env-ref $env_t41 $env k21))
      ((cps dict?)
       (env-ref $env_t41 $env b17)
       (make-closure
        (lambda-label $lambda43)
        (make-env
         $env_t41
         (b17 (env-ref $env_t41 $env b17))
         (i16 (env-ref $env_t41 $env i16))
         (k21 (env-ref $env_t41 $env k21))))))))
 (define-label
  $lambda45
  (lambda ($env rv22)
    (if rv22
      (error "Cannot delete from tuples!" (env-ref $env_t41 $env k21))
      ((cps py-list?)
       (env-ref $env_t41 $env b17)
       (make-closure
        (lambda-label $lambda44)
        (make-env
         $env_t41
         (b17 (env-ref $env_t41 $env b17))
         (i16 (env-ref $env_t41 $env i16))
         (k21 (env-ref $env_t41 $env k21))))))))
 (define-label
  $lambda46
  (lambda ($env i16 k21)
    ((cps tuple?)
     (env-ref $env_t42 $env b17)
     (make-closure
      (lambda-label $lambda45)
      (make-env
       $env_t41
       (b17 (env-ref $env_t42 $env b17))
       (i16 i16)
       (k21 k21))))))
 (define-env $env_t38 (b15 i14 k15))
 (define-env $env_t39 (b15))
 (define-env $env_t40 ())
 (define-env $env_t41 (b17 i16 k21))
 (define-env $env_t42 (b17))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  (dict (1 2) (3 4))
  (set-then!
   g$b
   (py-list* 0 1 2)
   (app*
    (make-closure (lambda-label $lambda42) (make-env $env_t40))
    g$a
    (make-closure (lambda-label $lambda50) (make-env $env_t40))))))
