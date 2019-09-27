(program
 (define-env $env_t40 (k16 return))
 (define-env $env_t41 (return))
 (define-env $env_t55 (e22 i21 k47))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t54 (b17 k22))
 (define-env $env_t46 (e20 i19 k32))
 (define-env $env_t47 (e20))
 (define-env $env_t48 (i16))
 (define-env $env_t57 (e22 k46))
 (define-env $env_t42 (k20 return))
 (define-env $env_t49 (b17 i16 k24))
 (define-env $env_t53 (b17))
 (define-env $env_t50 (b17 i16 k24 v18))
 (define-env $env_t51 (b17 i16))
 (define-env $env_t52 (b17 i16 k23))
 (define-env $env_t43 (e15 i14 k39))
 (define-env $env_t44 (e15))
 (define-env $env_t45 (e15 k38))
 (define-env $env_t56 (e22))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$g (void))
 (define g$a (void))
 (define g$f (void))
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
           (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x))
           (make-env $env_t38 (cc cc)))
          cc))
       (make-env $env_t39))
      (make-closure
       (lambda ($env return k15)
         (app*
          (make-closure
           (lambda ($env k16)
             ((cps py-print)
              "called f"
              (make-closure
               (lambda ($env rv17)
                 (app*
                  (env-ref $env_t40 $env return)
                  1
                  (env-ref $env_t40 $env k16)))
               (make-env
                $env_t40
                (k16 k16)
                (return (env-ref $env_t41 $env return))))))
           (make-env $env_t41 (return return)))
          k15))
       (make-env $env_t39))
      k14))
   (make-env $env_t39))
  (set-then!
   g$g
   (make-closure
    (lambda ($env k18)
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
        (lambda ($env return k19)
          (app*
           (make-closure
            (lambda ($env k20)
              ((cps py-print)
               "called g"
               (make-closure
                (lambda ($env rv21)
                  (app*
                   (env-ref $env_t42 $env return)
                   0
                   (env-ref $env_t42 $env k20)))
                (make-env
                 $env_t42
                 (k20 k20)
                 (return (env-ref $env_t41 $env return))))))
            (make-env $env_t41 (return return)))
           k19))
        (make-env $env_t39))
       k18))
    (make-env $env_t39))
   (set-then!
    g$a
    (py-list* (py-list* 10 20) (py-list* 30 40) (py-list* 50 60))
    (app*
     (make-closure
      (lambda ($env e15 k38)
        (app*
         g$f
         (make-closure
          (lambda ($env rv43)
            (app*
             (make-closure
              (lambda ($env i14 k39)
                ((cps py-list?)
                 (env-ref $env_t44 $env e15)
                 (make-closure
                  (lambda ($env rv40)
                    (if rv40
                      ((cps py-list-ref)
                       (env-ref $env_t43 $env e15)
                       (env-ref $env_t43 $env i14)
                       (env-ref $env_t43 $env k39))
                      ((cps tuple?)
                       (env-ref $env_t43 $env e15)
                       (make-closure
                        (lambda ($env rv41)
                          (if rv41
                            ((cps tuple-ref)
                             (env-ref $env_t43 $env e15)
                             (env-ref $env_t43 $env i14)
                             (env-ref $env_t43 $env k39))
                            ((cps dict?)
                             (env-ref $env_t43 $env e15)
                             (make-closure
                              (lambda ($env rv42)
                                (if rv42
                                  ((cps dict-ref)
                                   (env-ref $env_t43 $env e15)
                                   (env-ref $env_t43 $env i14)
                                   (env-ref $env_t43 $env k39))
                                  (error
                                   "cannot index object"
                                   (env-ref $env_t43 $env k39))))
                              (make-env
                               $env_t43
                               (e15 (env-ref $env_t43 $env e15))
                               (i14 (env-ref $env_t43 $env i14))
                               (k39 (env-ref $env_t43 $env k39)))))))
                        (make-env
                         $env_t43
                         (e15 (env-ref $env_t43 $env e15))
                         (i14 (env-ref $env_t43 $env i14))
                         (k39 (env-ref $env_t43 $env k39)))))))
                  (make-env
                   $env_t43
                   (e15 (env-ref $env_t44 $env e15))
                   (i14 i14)
                   (k39 k39)))))
              (make-env $env_t44 (e15 (env-ref $env_t45 $env e15))))
             rv43
             (env-ref $env_t45 $env k38)))
          (make-env $env_t45 (e15 e15) (k38 k38)))))
      (make-env $env_t39))
     g$a
     (make-closure
      (lambda ($env rv44)
        (app*
         (make-closure
          (lambda ($env b17 k22)
            (app*
             g$g
             (make-closure
              (lambda ($env rv37)
                (app*
                 (make-closure
                  (lambda ($env i16 k23)
                    (app*
                     (make-closure
                      (lambda ($env e20 k31)
                        (app*
                         (make-closure
                          (lambda ($env i19 k32)
                            ((cps py-list?)
                             (env-ref $env_t47 $env e20)
                             (make-closure
                              (lambda ($env rv33)
                                (if rv33
                                  ((cps py-list-ref)
                                   (env-ref $env_t46 $env e20)
                                   (env-ref $env_t46 $env i19)
                                   (env-ref $env_t46 $env k32))
                                  ((cps tuple?)
                                   (env-ref $env_t46 $env e20)
                                   (make-closure
                                    (lambda ($env rv34)
                                      (if rv34
                                        ((cps tuple-ref)
                                         (env-ref $env_t46 $env e20)
                                         (env-ref $env_t46 $env i19)
                                         (env-ref $env_t46 $env k32))
                                        ((cps dict?)
                                         (env-ref $env_t46 $env e20)
                                         (make-closure
                                          (lambda ($env rv35)
                                            (if rv35
                                              ((cps dict-ref)
                                               (env-ref $env_t46 $env e20)
                                               (env-ref $env_t46 $env i19)
                                               (env-ref $env_t46 $env k32))
                                              (error
                                               "cannot index object"
                                               (env-ref $env_t46 $env k32))))
                                          (make-env
                                           $env_t46
                                           (e20 (env-ref $env_t46 $env e20))
                                           (i19 (env-ref $env_t46 $env i19))
                                           (k32
                                            (env-ref $env_t46 $env k32)))))))
                                    (make-env
                                     $env_t46
                                     (e20 (env-ref $env_t46 $env e20))
                                     (i19 (env-ref $env_t46 $env i19))
                                     (k32 (env-ref $env_t46 $env k32)))))))
                              (make-env
                               $env_t46
                               (e20 (env-ref $env_t47 $env e20))
                               (i19 i19)
                               (k32 k32)))))
                          (make-env $env_t47 (e20 e20)))
                         (env-ref $env_t48 $env i16)
                         k31))
                      (make-env $env_t48 (i16 i16)))
                     (env-ref $env_t53 $env b17)
                     (make-closure
                      (lambda ($env rv36)
                        (app*
                         (make-closure
                          (lambda ($env v18 k24)
                            ((cps tuple?)
                             (env-ref $env_t51 $env b17)
                             (make-closure
                              (lambda ($env rv25)
                                (if rv25
                                  ((cps +)
                                   (env-ref $env_t50 $env v18)
                                   30
                                   (make-closure
                                    (lambda ($env rv26)
                                      ((cps tuple-set!)
                                       (env-ref $env_t49 $env b17)
                                       (env-ref $env_t49 $env i16)
                                       rv26
                                       (env-ref $env_t49 $env k24)))
                                    (make-env
                                     $env_t49
                                     (b17 (env-ref $env_t50 $env b17))
                                     (i16 (env-ref $env_t50 $env i16))
                                     (k24 (env-ref $env_t50 $env k24)))))
                                  ((cps py-list?)
                                   (env-ref $env_t50 $env b17)
                                   (make-closure
                                    (lambda ($env rv27)
                                      (if rv27
                                        ((cps +)
                                         (env-ref $env_t50 $env v18)
                                         30
                                         (make-closure
                                          (lambda ($env rv28)
                                            ((cps py-list-set!)
                                             (env-ref $env_t49 $env b17)
                                             (env-ref $env_t49 $env i16)
                                             rv28
                                             (env-ref $env_t49 $env k24)))
                                          (make-env
                                           $env_t49
                                           (b17 (env-ref $env_t50 $env b17))
                                           (i16 (env-ref $env_t50 $env i16))
                                           (k24 (env-ref $env_t50 $env k24)))))
                                        ((cps dict?)
                                         (env-ref $env_t50 $env b17)
                                         (make-closure
                                          (lambda ($env rv29)
                                            (if rv29
                                              ((cps +)
                                               (env-ref $env_t50 $env v18)
                                               30
                                               (make-closure
                                                (lambda ($env rv30)
                                                  ((cps dict-set!)
                                                   (env-ref $env_t49 $env b17)
                                                   (env-ref $env_t49 $env i16)
                                                   rv30
                                                   (env-ref
                                                    $env_t49
                                                    $env
                                                    k24)))
                                                (make-env
                                                 $env_t49
                                                 (b17
                                                  (env-ref $env_t50 $env b17))
                                                 (i16
                                                  (env-ref $env_t50 $env i16))
                                                 (k24
                                                  (env-ref
                                                   $env_t50
                                                   $env
                                                   k24)))))
                                              (app*
                                               (env-ref $env_t50 $env k24)
                                               (void))))
                                          (make-env
                                           $env_t50
                                           (b17 (env-ref $env_t50 $env b17))
                                           (i16 (env-ref $env_t50 $env i16))
                                           (k24 (env-ref $env_t50 $env k24))
                                           (v18
                                            (env-ref $env_t50 $env v18)))))))
                                    (make-env
                                     $env_t50
                                     (b17 (env-ref $env_t50 $env b17))
                                     (i16 (env-ref $env_t50 $env i16))
                                     (k24 (env-ref $env_t50 $env k24))
                                     (v18 (env-ref $env_t50 $env v18)))))))
                              (make-env
                               $env_t50
                               (b17 (env-ref $env_t51 $env b17))
                               (i16 (env-ref $env_t51 $env i16))
                               (k24 k24)
                               (v18 v18)))))
                          (make-env
                           $env_t51
                           (b17 (env-ref $env_t52 $env b17))
                           (i16 (env-ref $env_t52 $env i16))))
                         rv36
                         (env-ref $env_t52 $env k23)))
                      (make-env
                       $env_t52
                       (b17 (env-ref $env_t53 $env b17))
                       (i16 i16)
                       (k23 k23)))))
                  (make-env $env_t53 (b17 (env-ref $env_t54 $env b17))))
                 rv37
                 (env-ref $env_t54 $env k22)))
              (make-env $env_t54 (b17 b17) (k22 k22)))))
          (make-env $env_t39))
         rv44
         (make-closure
          (lambda ($env rv45)
            (app*
             (make-closure
              (lambda ($env e22 k46)
                (app*
                 g$f
                 (make-closure
                  (lambda ($env rv51)
                    (app*
                     (make-closure
                      (lambda ($env i21 k47)
                        ((cps py-list?)
                         (env-ref $env_t56 $env e22)
                         (make-closure
                          (lambda ($env rv48)
                            (if rv48
                              ((cps py-list-ref)
                               (env-ref $env_t55 $env e22)
                               (env-ref $env_t55 $env i21)
                               (env-ref $env_t55 $env k47))
                              ((cps tuple?)
                               (env-ref $env_t55 $env e22)
                               (make-closure
                                (lambda ($env rv49)
                                  (if rv49
                                    ((cps tuple-ref)
                                     (env-ref $env_t55 $env e22)
                                     (env-ref $env_t55 $env i21)
                                     (env-ref $env_t55 $env k47))
                                    ((cps dict?)
                                     (env-ref $env_t55 $env e22)
                                     (make-closure
                                      (lambda ($env rv50)
                                        (if rv50
                                          ((cps dict-ref)
                                           (env-ref $env_t55 $env e22)
                                           (env-ref $env_t55 $env i21)
                                           (env-ref $env_t55 $env k47))
                                          (error
                                           "cannot index object"
                                           (env-ref $env_t55 $env k47))))
                                      (make-env
                                       $env_t55
                                       (e22 (env-ref $env_t55 $env e22))
                                       (i21 (env-ref $env_t55 $env i21))
                                       (k47 (env-ref $env_t55 $env k47)))))))
                                (make-env
                                 $env_t55
                                 (e22 (env-ref $env_t55 $env e22))
                                 (i21 (env-ref $env_t55 $env i21))
                                 (k47 (env-ref $env_t55 $env k47)))))))
                          (make-env
                           $env_t55
                           (e22 (env-ref $env_t56 $env e22))
                           (i21 i21)
                           (k47 k47)))))
                      (make-env $env_t56 (e22 (env-ref $env_t57 $env e22))))
                     rv51
                     (env-ref $env_t57 $env k46)))
                  (make-env $env_t57 (e22 e22) (k46 k46)))))
              (make-env $env_t39))
             g$a
             (make-closure
              (lambda ($env rv52) ((cps py-print) rv52 $halt))
              (make-env $env_t39))))
          (make-env $env_t39))))
      (make-env $env_t39)))))))
