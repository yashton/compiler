(program
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$x (void))
 (set-then!
  g$x
  10
  (set-then!
   g$f
   (make-closure
    (lambda ($env k14)
      (app*
       (make-closure
        (lambda ($env f cc)
          (app*
           f
           (make-closure
            (lambda ($env x k)
              (set-then!
               x
               (make-cell x)
               (app* (env-ref $env_t38 $env cc) (get-cell x))))
            (make-env $env_t38 (cc cc)))
           cc))
        (make-env $env_t39))
       (make-closure
        (lambda ($env return k15)
          (app*
           (make-closure
            (lambda ($env x g k16)
              (set-then!
               x
               (make-cell x)
               (set-then!
                g
                (make-cell g)
                (set-cell!
                 x
                 20
                 (set-cell!
                  g
                  (make-closure
                   (lambda ($env k17)
                     (app*
                      (make-closure
                       (lambda ($env f cc)
                         (app*
                          f
                          (make-closure
                           (lambda ($env x k)
                             (set-then!
                              x
                              (make-cell x)
                              (app* (env-ref $env_t38 $env cc) (get-cell x))))
                           (make-env $env_t38 (cc cc)))
                          cc))
                       (make-env $env_t39))
                      (make-closure
                       (lambda ($env return k18)
                         (app*
                          (make-closure
                           (lambda ($env k19)
                             (set-then! g$x 30 (app* k19 (void))))
                           (make-env $env_t39))
                          k18))
                       (make-env $env_t39))
                      k17))
                   (make-env $env_t39))
                  (app* (get-cell g) k16))))))
            (make-env $env_t39))
           (void)
           (void)
           k15))
        (make-env $env_t39))
       k14))
    (make-env $env_t39))
   (app*
    g$f
    (make-closure
     (lambda ($env rv20) ((cps py-print) g$x $halt))
     (make-env $env_t39))))))
