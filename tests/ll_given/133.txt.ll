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
 (define-label $lambda40 (lambda ($env k24) ((cps py-print) 10 k24)))
 (define-label
  $lambda41
  (lambda ($env ex k23)
    (app* (make-closure (lambda-label $lambda40) (make-env $env_t39)) k23)))
 (define-label
  $lambda42
  (lambda ($env rv25)
    (app* (env-ref $env_t40 $env $ec15) rv25 (env-ref $env_t40 $env k22))))
 (define-label
  $lambda43
  (lambda ($env $ex14 k22)
    (set-then!
     $current-handler
     (env-ref $env_t41 $env $old-handler)
     (app*
      (make-closure (lambda-label $lambda41) (make-env $env_t39))
      $ex14
      (make-closure
       (lambda-label $lambda42)
       (make-env $env_t40 ($ec15 (env-ref $env_t41 $env $ec15)) (k22 k22)))))))
 (define-label $lambda44 (lambda ($env k27) ((cps py-print) 3 k27)))
 (define-label
  $lambda45
  (lambda ($env rv k26)
    (set-then!
     $current-handler
     (env-ref $env_t42 $env $old-handler)
     (app* k26 rv))))
 (define-label
  $lambda46
  (lambda ($env rv28)
    (app*
     (make-closure
      (lambda-label $lambda45)
      (make-env $env_t42 ($old-handler (env-ref $env_t43 $env $old-handler))))
     rv28
     (env-ref $env_t43 $env k21))))
 (define-label
  $lambda47
  (lambda ($env $ec15 k21)
    (set-then!
     $current-handler
     (make-closure
      (lambda-label $lambda43)
      (make-env
       $env_t41
       ($ec15 $ec15)
       ($old-handler (env-ref $env_t42 $env $old-handler))))
     (app*
      (make-closure (lambda-label $lambda44) (make-env $env_t39))
      (make-closure
       (lambda-label $lambda46)
       (make-env
        $env_t43
        ($old-handler (env-ref $env_t42 $env $old-handler))
        (k21 k21)))))))
 (define-label
  $lambda48
  (lambda ($env break k20)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure
      (lambda-label $lambda47)
      (make-env $env_t42 ($old-handler (env-ref $env_t42 $env $old-handler))))
     k20)))
 (define-label
  $lambda49
  (lambda ($env k29)
    (set-then!
     $current-handler
     (env-ref $env_t44 $env $old-handler)
     (app* (env-ref $env_t44 $env $old-break) k29))))
 (define-label
  $lambda50
  (lambda ($env continue k19)
    (app*
     (make-closure
      (lambda-label $lambda48)
      (make-env $env_t42 ($old-handler (env-ref $env_t44 $env $old-handler))))
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t44
       ($old-break (env-ref $env_t44 $env $old-break))
       ($old-handler (env-ref $env_t44 $env $old-handler))))
     k19)))
 (define-label
  $lambda51
  (lambda ($env k30)
    (set-then!
     $current-handler
     (env-ref $env_t45 $env $old-handler)
     (app* (env-ref $env_t45 $env $old-continue) k30))))
 (define-label
  $lambda52
  (lambda ($env return k18)
    (app*
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t44
       ($old-break (env-ref $env_t46 $env $old-break))
       ($old-handler (env-ref $env_t46 $env $old-handler))))
     (make-closure
      (lambda-label $lambda51)
      (make-env
       $env_t45
       ($old-continue (env-ref $env_t46 $env $old-continue))
       ($old-handler (env-ref $env_t46 $env $old-handler))))
     k18)))
 (define-label
  $lambda53
  (lambda ($env rv k31)
    (set-then!
     $current-handler
     (env-ref $env_t42 $env $old-handler)
     (app* return rv k31))))
 (define-label
  $lambda54
  (lambda ($env $old-break k17)
    (app*
     (make-closure
      (lambda-label $lambda52)
      (make-env
       $env_t46
       ($old-break $old-break)
       ($old-continue (env-ref $env_t45 $env $old-continue))
       ($old-handler (env-ref $env_t45 $env $old-handler))))
     (make-closure
      (lambda-label $lambda53)
      (make-env $env_t42 ($old-handler (env-ref $env_t45 $env $old-handler))))
     k17)))
 (define-label
  $lambda55
  (lambda ($env $old-continue k16)
    (app*
     (make-closure
      (lambda-label $lambda54)
      (make-env
       $env_t45
       ($old-continue $old-continue)
       ($old-handler (env-ref $env_t42 $env $old-handler))))
     break
     k16)))
 (define-label
  $lambda56
  (lambda ($env $old-return k15)
    (app*
     (make-closure
      (lambda-label $lambda55)
      (make-env $env_t42 ($old-handler (env-ref $env_t42 $env $old-handler))))
     continue
     k15)))
 (define-label
  $lambda57
  (lambda ($env $old-handler k14)
    (app*
     (make-closure
      (lambda-label $lambda56)
      (make-env $env_t42 ($old-handler $old-handler)))
     return
     k14)))
 (define-env $env_t42 ($old-handler))
 (define-env $env_t43 ($old-handler k21))
 (define-env $env_t44 ($old-break $old-handler))
 (define-env $env_t45 ($old-continue $old-handler))
 (define-env $env_t46 ($old-break $old-continue $old-handler))
 (define-env $env_t40 ($ec15 k22))
 (define-env $env_t41 ($ec15 $old-handler))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (app*
  (make-closure (lambda-label $lambda57) (make-env $env_t39))
  $current-handler
  $halt))
