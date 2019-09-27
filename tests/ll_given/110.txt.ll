(program
 (define-label
  $lambda40
  (lambda ($env rv14)
    ((cps bitwise-and)
     1
     2
     3
     4
     5
     (make-closure (lambda-label $lambda39) (make-env $env_t39 (rv14 rv14))))))
 (define-label
  $lambda38
  (lambda ($env rv16)
    (set-then!
     g$a
     (tuple (env-ref $env_t38 $env rv14) (env-ref $env_t38 $env rv15) rv16)
     (app* $halt (void)))))
 (define-label
  $lambda39
  (lambda ($env rv15)
    ((cps bitwise-or)
     1
     2
     3
     4
     5
     (make-closure
      (lambda-label $lambda38)
      (make-env $env_t38 (rv14 (env-ref $env_t39 $env rv14)) (rv15 rv15))))))
 (define-env $env_t38 (rv14 rv15))
 (define-env $env_t39 (rv14))
 (define-env $env_t40 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 ((cps bitwise-xor)
  1
  2
  3
  4
  5
  (make-closure (lambda-label $lambda40) (make-env $env_t40))))
