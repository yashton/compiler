(program
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$b (void))
 (define g$c (void))
 (define g$d (void))
 (define g$g (void))
 (define g$f (void))
 ((cps +)
  1
  1
  (make-closure
   (lambda ($env rv14)
     (set-then!
      g$a
      rv14
      ((cps modulo)
       2
       3
       (make-closure
        (lambda ($env rv15)
          (set-then!
           g$b
           rv15
           ((cps bitwise-xor)
            1
            1
            (make-closure
             (lambda ($env rv16)
               (set-then!
                g$c
                rv16
                ((cps -)
                 1
                 1
                 (make-closure
                  (lambda ($env rv17)
                    (set-then!
                     g$d
                     rv17
                     ((cps *)
                      1
                      1
                      (make-closure
                       (lambda ($env rv18)
                         (set-then!
                          g$f
                          rv18
                          ((cps /)
                           1
                           1
                           (make-closure
                            (lambda ($env rv19)
                              (set-then! g$g rv19 (app* $halt (void))))
                            (make-env $env_t38)))))
                       (make-env $env_t38)))))
                  (make-env $env_t38)))))
             (make-env $env_t38)))))
        (make-env $env_t38)))))
   (make-env $env_t38))))
