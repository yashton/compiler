(program
 (define-env $env_t38 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$s (void))
 (app*
  get-field
  g$a
  f
  (make-closure
   (lambda ($env rv14)
     (app*
      get-field
      g$b
      fg
      (make-closure
       (lambda ($env rv15)
         (app*
          rv15
          (make-closure
           (lambda ($env rv16)
             ((cps +)
              1
              0+2.0i
              (make-closure
               (lambda ($env rv17) (set-then! g$s rv17 (app* $halt (void))))
               (make-env $env_t38))))
           (make-env $env_t38))))
       (make-env $env_t38))))
   (make-env $env_t38))))
