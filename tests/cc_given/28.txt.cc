(program
 (define-env $env_t41 (k28))
 (define-env $env_t42 (k21))
 (define-env $env_t43 (k18 loop))
 (define-env $env_t44 (loop))
 (define-env $env_t45 (k17))
 (define-env $env_t40 (k23))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$length (void))
 (define g$shape (void))
 (define g$width (void))
 (app*
  g$input
  g$menu
  (make-closure
   (lambda ($env rv14)
     (app*
      g$int
      rv14
      (make-closure
       (lambda ($env rv15)
         (set-then!
          g$shape
          rv15
          (app*
           (make-closure
            (lambda ($env f cc)
              (app*
               f
               (make-closure
                (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x))
                (make-env $env_t38 (cc cc)))
               cc))
            (make-env $env_t39))
           (make-closure
            (lambda ($env break k16)
              (app*
               (make-closure
                (lambda ($env loop k17)
                  (set-then!
                   loop
                   (make-cell loop)
                   (set-cell!
                    loop
                    (make-closure
                     (lambda ($env k18)
                       ((cps not-equal?)
                        g$shape
                        4
                        (make-closure
                         (lambda ($env rv19)
                           (if rv19
                             (app*
                              (make-closure
                               (lambda ($env f cc)
                                 (app*
                                  f
                                  (make-closure
                                   (lambda ($env x k)
                                     (app* (env-ref $env_t38 $env cc) x))
                                   (make-env $env_t38 (cc cc)))
                                  cc))
                               (make-env $env_t39))
                              (make-closure
                               (lambda ($env continue k20)
                                 (app*
                                  (make-closure
                                   (lambda ($env k21)
                                     ((cps equal?)
                                      g$shape
                                      1
                                      (make-closure
                                       (lambda ($env rv22)
                                         (if rv22
                                           (app*
                                            (make-closure
                                             (lambda ($env k23)
                                               (app*
                                                g$input
                                                "Length: "
                                                (make-closure
                                                 (lambda ($env rv24)
                                                   (app*
                                                    g$float
                                                    rv24
                                                    (make-closure
                                                     (lambda ($env rv25)
                                                       (set-then!
                                                        g$length
                                                        rv25
                                                        ((cps expt)
                                                         g$length
                                                         2
                                                         (make-closure
                                                          (lambda ($env rv26)
                                                            ((cps py-print)
                                                             "Area of square = "
                                                             rv26
                                                             (env-ref
                                                              $env_t40
                                                              $env
                                                              k23)))
                                                          (make-env
                                                           $env_t40
                                                           (k23
                                                            (env-ref
                                                             $env_t40
                                                             $env
                                                             k23)))))))
                                                     (make-env
                                                      $env_t40
                                                      (k23
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        k23))))))
                                                 (make-env
                                                  $env_t40
                                                  (k23 k23)))))
                                             (make-env $env_t39))
                                            (env-ref $env_t42 $env k21))
                                           ((cps equal?)
                                            g$shape
                                            2
                                            (make-closure
                                             (lambda ($env rv27)
                                               (if rv27
                                                 (app*
                                                  (make-closure
                                                   (lambda ($env k28)
                                                     (app*
                                                      g$input
                                                      "Length: "
                                                      (make-closure
                                                       (lambda ($env rv29)
                                                         (app*
                                                          g$float
                                                          rv29
                                                          (make-closure
                                                           (lambda ($env rv30)
                                                             (set-then!
                                                              g$length
                                                              rv30
                                                              (app*
                                                               g$input
                                                               "Width: "
                                                               (make-closure
                                                                (lambda ($env
                                                                         rv31)
                                                                  (app*
                                                                   g$float
                                                                   rv31
                                                                   (make-closure
                                                                    (lambda ($env
                                                                             rv32)
                                                                      (set-then!
                                                                       g$width
                                                                       rv32
                                                                       ((cps *)
                                                                        g$length
                                                                        g$width
                                                                        (make-closure
                                                                         (lambda ($env
                                                                                  rv33)
                                                                           ((cps
                                                                             py-print)
                                                                            "Area of rectangle = "
                                                                            rv33
                                                                            (env-ref
                                                                             $env_t41
                                                                             $env
                                                                             k28)))
                                                                         (make-env
                                                                          $env_t41
                                                                          (k28
                                                                           (env-ref
                                                                            $env_t41
                                                                            $env
                                                                            k28)))))))
                                                                    (make-env
                                                                     $env_t41
                                                                     (k28
                                                                      (env-ref
                                                                       $env_t41
                                                                       $env
                                                                       k28))))))
                                                                (make-env
                                                                 $env_t41
                                                                 (k28
                                                                  (env-ref
                                                                   $env_t41
                                                                   $env
                                                                   k28)))))))
                                                           (make-env
                                                            $env_t41
                                                            (k28
                                                             (env-ref
                                                              $env_t41
                                                              $env
                                                              k28))))))
                                                       (make-env
                                                        $env_t41
                                                        (k28 k28)))))
                                                   (make-env $env_t39))
                                                  (env-ref $env_t42 $env k21))
                                                 (app*
                                                  (make-closure
                                                   (lambda ($env k34)
                                                     ((cps py-print)
                                                      " Not a valid shape. try again"
                                                      k34))
                                                   (make-env $env_t39))
                                                  (env-ref
                                                   $env_t42
                                                   $env
                                                   k21))))
                                             (make-env
                                              $env_t42
                                              (k21
                                               (env-ref
                                                $env_t42
                                                $env
                                                k21)))))))
                                       (make-env $env_t42 (k21 k21)))))
                                   (make-env $env_t39))
                                  k20))
                               (make-env $env_t39))
                              (make-closure
                               (lambda ($env rv35)
                                 (app*
                                  (get-cell (env-ref $env_t43 $env loop))
                                  (env-ref $env_t43 $env k18)))
                               (make-env
                                $env_t43
                                (k18 (env-ref $env_t43 $env k18))
                                (loop (env-ref $env_t43 $env loop)))))
                             (app* (env-ref $env_t43 $env k18) (void))))
                         (make-env
                          $env_t43
                          (k18 k18)
                          (loop (env-ref $env_t44 $env loop))))))
                     (make-env $env_t44 (loop loop)))
                    (app*
                     (get-cell loop)
                     (make-closure
                      (lambda ($env rv36)
                        (app* (env-ref $env_t45 $env k17) (void)))
                      (make-env $env_t45 (k17 k17)))))))
                (make-env $env_t39))
               (void)
               k16))
            (make-env $env_t39))
           $halt)))
       (make-env $env_t39))))
   (make-env $env_t39))))
