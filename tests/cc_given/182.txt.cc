(program
 (define-env $env_t52 (e19 i18 k41))
 (define-env $env_t53 (e19))
 (define-env $env_t54 (k16 return))
 (define-env $env_t51 (b17 n))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t47 (b17 i16 k20 rv23))
 (define-env $env_t46 (b17 i16 k20))
 (define-env $env_t55 (k16 n return))
 (define-env $env_t48 (b17 i16 k20 n))
 (define-env $env_t49 (b17 i16 k20 rv29))
 (define-env $env_t40 (e15 i14 k49))
 (define-env $env_t41 (e15))
 (define-env $env_t42 (n))
 (define-env $env_t50 (b17 i16 k20 rv35))
 (define-env $env_t43 (k47 return))
 (define-env $env_t44 (n return))
 (define-env $env_t45 (k17 n return))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fib (void))
 (define g$cache (void))
 (set-then!
  g$cache
  (dict (0 0) (1 1))
  (set-then!
   g$fib
   (make-closure
    (lambda ($env n k14)
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
        (lambda ($env return k15)
          (app*
           (make-closure
            (lambda ($env k16)
              (app*
               (make-closure
                (lambda ($env k17)
                  ((cps in?)
                   (env-ref $env_t44 $env n)
                   g$cache
                   (make-closure
                    (lambda ($env rv46)
                      (if rv46
                        (app*
                         (make-closure
                          (lambda ($env k47)
                            (app*
                             (make-closure
                              (lambda ($env e15 k48)
                                (app*
                                 (make-closure
                                  (lambda ($env i14 k49)
                                    ((cps py-list?)
                                     (env-ref $env_t41 $env e15)
                                     (make-closure
                                      (lambda ($env rv50)
                                        (if rv50
                                          ((cps py-list-ref)
                                           (env-ref $env_t40 $env e15)
                                           (env-ref $env_t40 $env i14)
                                           (env-ref $env_t40 $env k49))
                                          ((cps tuple?)
                                           (env-ref $env_t40 $env e15)
                                           (make-closure
                                            (lambda ($env rv51)
                                              (if rv51
                                                ((cps tuple-ref)
                                                 (env-ref $env_t40 $env e15)
                                                 (env-ref $env_t40 $env i14)
                                                 (env-ref $env_t40 $env k49))
                                                ((cps dict?)
                                                 (env-ref $env_t40 $env e15)
                                                 (make-closure
                                                  (lambda ($env rv52)
                                                    (if rv52
                                                      ((cps dict-ref)
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        e15)
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        i14)
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        k49))
                                                      (error
                                                       "cannot index object"
                                                       (env-ref
                                                        $env_t40
                                                        $env
                                                        k49))))
                                                  (make-env
                                                   $env_t40
                                                   (e15
                                                    (env-ref
                                                     $env_t40
                                                     $env
                                                     e15))
                                                   (i14
                                                    (env-ref
                                                     $env_t40
                                                     $env
                                                     i14))
                                                   (k49
                                                    (env-ref
                                                     $env_t40
                                                     $env
                                                     k49)))))))
                                            (make-env
                                             $env_t40
                                             (e15 (env-ref $env_t40 $env e15))
                                             (i14 (env-ref $env_t40 $env i14))
                                             (k49
                                              (env-ref $env_t40 $env k49)))))))
                                      (make-env
                                       $env_t40
                                       (e15 (env-ref $env_t41 $env e15))
                                       (i14 i14)
                                       (k49 k49)))))
                                  (make-env $env_t41 (e15 e15)))
                                 (env-ref $env_t42 $env n)
                                 k48))
                              (make-env
                               $env_t42
                               (n (env-ref $env_t44 $env n))))
                             g$cache
                             (make-closure
                              (lambda ($env rv53)
                                (app*
                                 (env-ref $env_t43 $env return)
                                 rv53
                                 (env-ref $env_t43 $env k47)))
                              (make-env
                               $env_t43
                               (k47 k47)
                               (return (env-ref $env_t44 $env return))))))
                          (make-env
                           $env_t44
                           (n (env-ref $env_t45 $env n))
                           (return (env-ref $env_t45 $env return))))
                         (env-ref $env_t45 $env k17))
                        (app* (env-ref $env_t45 $env k17) (void))))
                    (make-env
                     $env_t45
                     (k17 k17)
                     (n (env-ref $env_t44 $env n))
                     (return (env-ref $env_t44 $env return))))))
                (make-env
                 $env_t44
                 (n (env-ref $env_t44 $env n))
                 (return (env-ref $env_t44 $env return))))
               (make-closure
                (lambda ($env rv18)
                  (app*
                   (make-closure
                    (lambda ($env b17 k19)
                      (app*
                       (make-closure
                        (lambda ($env i16 k20)
                          ((cps tuple?)
                           (env-ref $env_t51 $env b17)
                           (make-closure
                            (lambda ($env rv21)
                              (if rv21
                                ((cps -)
                                 (env-ref $env_t48 $env n)
                                 1
                                 (make-closure
                                  (lambda ($env rv22)
                                    (app*
                                     g$fib
                                     rv22
                                     (make-closure
                                      (lambda ($env rv23)
                                        ((cps -)
                                         (env-ref $env_t48 $env n)
                                         2
                                         (make-closure
                                          (lambda ($env rv24)
                                            (app*
                                             g$fib
                                             rv24
                                             (make-closure
                                              (lambda ($env rv25)
                                                ((cps +)
                                                 (env-ref $env_t47 $env rv23)
                                                 rv25
                                                 (make-closure
                                                  (lambda ($env rv26)
                                                    ((cps tuple-set!)
                                                     (env-ref
                                                      $env_t46
                                                      $env
                                                      b17)
                                                     (env-ref
                                                      $env_t46
                                                      $env
                                                      i16)
                                                     rv26
                                                     (env-ref
                                                      $env_t46
                                                      $env
                                                      k20)))
                                                  (make-env
                                                   $env_t46
                                                   (b17
                                                    (env-ref
                                                     $env_t47
                                                     $env
                                                     b17))
                                                   (i16
                                                    (env-ref
                                                     $env_t47
                                                     $env
                                                     i16))
                                                   (k20
                                                    (env-ref
                                                     $env_t47
                                                     $env
                                                     k20))))))
                                              (make-env
                                               $env_t47
                                               (b17
                                                (env-ref $env_t47 $env b17))
                                               (i16
                                                (env-ref $env_t47 $env i16))
                                               (k20
                                                (env-ref $env_t47 $env k20))
                                               (rv23
                                                (env-ref
                                                 $env_t47
                                                 $env
                                                 rv23))))))
                                          (make-env
                                           $env_t47
                                           (b17 (env-ref $env_t48 $env b17))
                                           (i16 (env-ref $env_t48 $env i16))
                                           (k20 (env-ref $env_t48 $env k20))
                                           (rv23 rv23)))))
                                      (make-env
                                       $env_t48
                                       (b17 (env-ref $env_t48 $env b17))
                                       (i16 (env-ref $env_t48 $env i16))
                                       (k20 (env-ref $env_t48 $env k20))
                                       (n (env-ref $env_t48 $env n))))))
                                  (make-env
                                   $env_t48
                                   (b17 (env-ref $env_t48 $env b17))
                                   (i16 (env-ref $env_t48 $env i16))
                                   (k20 (env-ref $env_t48 $env k20))
                                   (n (env-ref $env_t48 $env n)))))
                                ((cps py-list?)
                                 (env-ref $env_t48 $env b17)
                                 (make-closure
                                  (lambda ($env rv27)
                                    (if rv27
                                      ((cps -)
                                       (env-ref $env_t48 $env n)
                                       1
                                       (make-closure
                                        (lambda ($env rv28)
                                          (app*
                                           g$fib
                                           rv28
                                           (make-closure
                                            (lambda ($env rv29)
                                              ((cps -)
                                               (env-ref $env_t48 $env n)
                                               2
                                               (make-closure
                                                (lambda ($env rv30)
                                                  (app*
                                                   g$fib
                                                   rv30
                                                   (make-closure
                                                    (lambda ($env rv31)
                                                      ((cps +)
                                                       (env-ref
                                                        $env_t49
                                                        $env
                                                        rv29)
                                                       rv31
                                                       (make-closure
                                                        (lambda ($env rv32)
                                                          ((cps py-list-set!)
                                                           (env-ref
                                                            $env_t46
                                                            $env
                                                            b17)
                                                           (env-ref
                                                            $env_t46
                                                            $env
                                                            i16)
                                                           rv32
                                                           (env-ref
                                                            $env_t46
                                                            $env
                                                            k20)))
                                                        (make-env
                                                         $env_t46
                                                         (b17
                                                          (env-ref
                                                           $env_t49
                                                           $env
                                                           b17))
                                                         (i16
                                                          (env-ref
                                                           $env_t49
                                                           $env
                                                           i16))
                                                         (k20
                                                          (env-ref
                                                           $env_t49
                                                           $env
                                                           k20))))))
                                                    (make-env
                                                     $env_t49
                                                     (b17
                                                      (env-ref
                                                       $env_t49
                                                       $env
                                                       b17))
                                                     (i16
                                                      (env-ref
                                                       $env_t49
                                                       $env
                                                       i16))
                                                     (k20
                                                      (env-ref
                                                       $env_t49
                                                       $env
                                                       k20))
                                                     (rv29
                                                      (env-ref
                                                       $env_t49
                                                       $env
                                                       rv29))))))
                                                (make-env
                                                 $env_t49
                                                 (b17
                                                  (env-ref $env_t48 $env b17))
                                                 (i16
                                                  (env-ref $env_t48 $env i16))
                                                 (k20
                                                  (env-ref $env_t48 $env k20))
                                                 (rv29 rv29)))))
                                            (make-env
                                             $env_t48
                                             (b17 (env-ref $env_t48 $env b17))
                                             (i16 (env-ref $env_t48 $env i16))
                                             (k20 (env-ref $env_t48 $env k20))
                                             (n (env-ref $env_t48 $env n))))))
                                        (make-env
                                         $env_t48
                                         (b17 (env-ref $env_t48 $env b17))
                                         (i16 (env-ref $env_t48 $env i16))
                                         (k20 (env-ref $env_t48 $env k20))
                                         (n (env-ref $env_t48 $env n)))))
                                      ((cps dict?)
                                       (env-ref $env_t48 $env b17)
                                       (make-closure
                                        (lambda ($env rv33)
                                          (if rv33
                                            ((cps -)
                                             (env-ref $env_t48 $env n)
                                             1
                                             (make-closure
                                              (lambda ($env rv34)
                                                (app*
                                                 g$fib
                                                 rv34
                                                 (make-closure
                                                  (lambda ($env rv35)
                                                    ((cps -)
                                                     (env-ref $env_t48 $env n)
                                                     2
                                                     (make-closure
                                                      (lambda ($env rv36)
                                                        (app*
                                                         g$fib
                                                         rv36
                                                         (make-closure
                                                          (lambda ($env rv37)
                                                            ((cps +)
                                                             (env-ref
                                                              $env_t50
                                                              $env
                                                              rv35)
                                                             rv37
                                                             (make-closure
                                                              (lambda ($env
                                                                       rv38)
                                                                ((cps
                                                                  dict-set!)
                                                                 (env-ref
                                                                  $env_t46
                                                                  $env
                                                                  b17)
                                                                 (env-ref
                                                                  $env_t46
                                                                  $env
                                                                  i16)
                                                                 rv38
                                                                 (env-ref
                                                                  $env_t46
                                                                  $env
                                                                  k20)))
                                                              (make-env
                                                               $env_t46
                                                               (b17
                                                                (env-ref
                                                                 $env_t50
                                                                 $env
                                                                 b17))
                                                               (i16
                                                                (env-ref
                                                                 $env_t50
                                                                 $env
                                                                 i16))
                                                               (k20
                                                                (env-ref
                                                                 $env_t50
                                                                 $env
                                                                 k20))))))
                                                          (make-env
                                                           $env_t50
                                                           (b17
                                                            (env-ref
                                                             $env_t50
                                                             $env
                                                             b17))
                                                           (i16
                                                            (env-ref
                                                             $env_t50
                                                             $env
                                                             i16))
                                                           (k20
                                                            (env-ref
                                                             $env_t50
                                                             $env
                                                             k20))
                                                           (rv35
                                                            (env-ref
                                                             $env_t50
                                                             $env
                                                             rv35))))))
                                                      (make-env
                                                       $env_t50
                                                       (b17
                                                        (env-ref
                                                         $env_t48
                                                         $env
                                                         b17))
                                                       (i16
                                                        (env-ref
                                                         $env_t48
                                                         $env
                                                         i16))
                                                       (k20
                                                        (env-ref
                                                         $env_t48
                                                         $env
                                                         k20))
                                                       (rv35 rv35)))))
                                                  (make-env
                                                   $env_t48
                                                   (b17
                                                    (env-ref
                                                     $env_t48
                                                     $env
                                                     b17))
                                                   (i16
                                                    (env-ref
                                                     $env_t48
                                                     $env
                                                     i16))
                                                   (k20
                                                    (env-ref
                                                     $env_t48
                                                     $env
                                                     k20))
                                                   (n
                                                    (env-ref
                                                     $env_t48
                                                     $env
                                                     n))))))
                                              (make-env
                                               $env_t48
                                               (b17
                                                (env-ref $env_t48 $env b17))
                                               (i16
                                                (env-ref $env_t48 $env i16))
                                               (k20
                                                (env-ref $env_t48 $env k20))
                                               (n (env-ref $env_t48 $env n)))))
                                            (app*
                                             (env-ref $env_t48 $env k20)
                                             (void))))
                                        (make-env
                                         $env_t48
                                         (b17 (env-ref $env_t48 $env b17))
                                         (i16 (env-ref $env_t48 $env i16))
                                         (k20 (env-ref $env_t48 $env k20))
                                         (n (env-ref $env_t48 $env n)))))))
                                  (make-env
                                   $env_t48
                                   (b17 (env-ref $env_t48 $env b17))
                                   (i16 (env-ref $env_t48 $env i16))
                                   (k20 (env-ref $env_t48 $env k20))
                                   (n (env-ref $env_t48 $env n)))))))
                            (make-env
                             $env_t48
                             (b17 (env-ref $env_t51 $env b17))
                             (i16 i16)
                             (k20 k20)
                             (n (env-ref $env_t51 $env n))))))
                        (make-env
                         $env_t51
                         (b17 b17)
                         (n (env-ref $env_t42 $env n))))
                       (env-ref $env_t42 $env n)
                       k19))
                    (make-env $env_t42 (n (env-ref $env_t55 $env n))))
                   g$cache
                   (make-closure
                    (lambda ($env rv39)
                      (app*
                       (make-closure
                        (lambda ($env e19 k40)
                          (app*
                           (make-closure
                            (lambda ($env i18 k41)
                              ((cps py-list?)
                               (env-ref $env_t53 $env e19)
                               (make-closure
                                (lambda ($env rv42)
                                  (if rv42
                                    ((cps py-list-ref)
                                     (env-ref $env_t52 $env e19)
                                     (env-ref $env_t52 $env i18)
                                     (env-ref $env_t52 $env k41))
                                    ((cps tuple?)
                                     (env-ref $env_t52 $env e19)
                                     (make-closure
                                      (lambda ($env rv43)
                                        (if rv43
                                          ((cps tuple-ref)
                                           (env-ref $env_t52 $env e19)
                                           (env-ref $env_t52 $env i18)
                                           (env-ref $env_t52 $env k41))
                                          ((cps dict?)
                                           (env-ref $env_t52 $env e19)
                                           (make-closure
                                            (lambda ($env rv44)
                                              (if rv44
                                                ((cps dict-ref)
                                                 (env-ref $env_t52 $env e19)
                                                 (env-ref $env_t52 $env i18)
                                                 (env-ref $env_t52 $env k41))
                                                (error
                                                 "cannot index object"
                                                 (env-ref $env_t52 $env k41))))
                                            (make-env
                                             $env_t52
                                             (e19 (env-ref $env_t52 $env e19))
                                             (i18 (env-ref $env_t52 $env i18))
                                             (k41
                                              (env-ref $env_t52 $env k41)))))))
                                      (make-env
                                       $env_t52
                                       (e19 (env-ref $env_t52 $env e19))
                                       (i18 (env-ref $env_t52 $env i18))
                                       (k41 (env-ref $env_t52 $env k41)))))))
                                (make-env
                                 $env_t52
                                 (e19 (env-ref $env_t53 $env e19))
                                 (i18 i18)
                                 (k41 k41)))))
                            (make-env $env_t53 (e19 e19)))
                           (env-ref $env_t42 $env n)
                           k40))
                        (make-env $env_t42 (n (env-ref $env_t55 $env n))))
                       g$cache
                       (make-closure
                        (lambda ($env rv45)
                          (app*
                           (env-ref $env_t54 $env return)
                           rv45
                           (env-ref $env_t54 $env k16)))
                        (make-env
                         $env_t54
                         (k16 (env-ref $env_t55 $env k16))
                         (return (env-ref $env_t55 $env return))))))
                    (make-env
                     $env_t55
                     (k16 (env-ref $env_t55 $env k16))
                     (n (env-ref $env_t55 $env n))
                     (return (env-ref $env_t55 $env return))))))
                (make-env
                 $env_t55
                 (k16 k16)
                 (n (env-ref $env_t44 $env n))
                 (return (env-ref $env_t44 $env return))))))
            (make-env $env_t44 (n (env-ref $env_t42 $env n)) (return return)))
           k15))
        (make-env $env_t42 (n n)))
       k14))
    (make-env $env_t39))
   (app*
    g$fib
    25
    (make-closure
     (lambda ($env rv54) ((cps py-print) rv54 $halt))
     (make-env $env_t39))))))
