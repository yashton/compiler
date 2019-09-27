(program
 (define-env $env_t43 (count k29))
 (define-env $env_t44 (count))
 (define-env $env_t45 (count k27))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t48 (ch count str))
 (define-env $env_t46 (ch count))
 (define-env $env_t47 (ch count i14))
 (define-env $env_t49 (return))
 (define-env $env_t50 (k16 return))
 (define-env $env_t51 (count k16 return))
 (define-env $env_t52 (return str))
 (define-env $env_t53 (str))
 (define-env $env_t40 ($loop15 $seq14 k19))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t42 (k18))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$two_e (void))
 (set-then!
  g$two_e
  (make-closure
   (lambda ($env str k14)
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
           (lambda ($env count ch k16)
             (set-then!
              count
              (make-cell count)
              (set-then!
               ch
               (make-cell ch)
               (set-cell!
                count
                0
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
                  (lambda ($env break k17)
                    (app*
                     (make-closure
                      (lambda ($env $seq14 $loop15 k18)
                        (app*
                         (make-closure
                          (lambda ($env k19)
                            ((cps set?)
                             (env-ref $env_t41 $env $seq14)
                             (make-closure
                              (lambda ($env rv21)
                                (if rv21
                                  (app*
                                   for-set-k
                                   (env-ref $env_t40 $env $seq14)
                                   (env-ref $env_t40 $env $loop15)
                                   (env-ref $env_t40 $env k19))
                                  ((cps tuple?)
                                   (env-ref $env_t40 $env $seq14)
                                   (make-closure
                                    (lambda ($env rv22)
                                      (if rv22
                                        (app*
                                         for-tuple-k
                                         (env-ref $env_t40 $env $seq14)
                                         (env-ref $env_t40 $env $loop15)
                                         (env-ref $env_t40 $env k19))
                                        ((cps py-list?)
                                         (env-ref $env_t40 $env $seq14)
                                         (make-closure
                                          (lambda ($env rv23)
                                            (if rv23
                                              (app*
                                               for-py-list-k
                                               (env-ref $env_t40 $env $seq14)
                                               (env-ref $env_t40 $env $loop15)
                                               (env-ref $env_t40 $env k19))
                                              ((cps dict?)
                                               (env-ref $env_t40 $env $seq14)
                                               (make-closure
                                                (lambda ($env rv24)
                                                  (if rv24
                                                    (app*
                                                     for-dict-k
                                                     (env-ref
                                                      $env_t40
                                                      $env
                                                      $seq14)
                                                     (env-ref
                                                      $env_t40
                                                      $env
                                                      $loop15)
                                                     (env-ref
                                                      $env_t40
                                                      $env
                                                      k19))
                                                    (app*
                                                     (env-ref
                                                      $env_t40
                                                      $env
                                                      k19)
                                                     (void))))
                                                (make-env
                                                 $env_t40
                                                 ($loop15
                                                  (env-ref
                                                   $env_t40
                                                   $env
                                                   $loop15))
                                                 ($seq14
                                                  (env-ref
                                                   $env_t40
                                                   $env
                                                   $seq14))
                                                 (k19
                                                  (env-ref
                                                   $env_t40
                                                   $env
                                                   k19)))))))
                                          (make-env
                                           $env_t40
                                           ($loop15
                                            (env-ref $env_t40 $env $loop15))
                                           ($seq14
                                            (env-ref $env_t40 $env $seq14))
                                           (k19
                                            (env-ref $env_t40 $env k19)))))))
                                    (make-env
                                     $env_t40
                                     ($loop15 (env-ref $env_t40 $env $loop15))
                                     ($seq14 (env-ref $env_t40 $env $seq14))
                                     (k19 (env-ref $env_t40 $env k19)))))))
                              (make-env
                               $env_t40
                               ($loop15 (env-ref $env_t41 $env $loop15))
                               ($seq14 (env-ref $env_t41 $env $seq14))
                               (k19 k19)))))
                          (make-env
                           $env_t41
                           ($loop15 $loop15)
                           ($seq14 $seq14)))
                         (make-closure
                          (lambda ($env rv20)
                            (app* (env-ref $env_t42 $env k18) (void)))
                          (make-env $env_t42 (k18 k18)))))
                      (make-env $env_t39))
                     (env-ref $env_t48 $env str)
                     (make-closure
                      (lambda ($env i14 k25)
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
                          (lambda ($env continue k26)
                            (set-cell!
                             (env-ref $env_t47 $env ch)
                             (env-ref $env_t47 $env i14)
                             (app*
                              (make-closure
                               (lambda ($env k27)
                                 ((cps equal?)
                                  (get-cell (env-ref $env_t46 $env ch))
                                  "e"
                                  (make-closure
                                   (lambda ($env rv28)
                                     (if rv28
                                       (app*
                                        (make-closure
                                         (lambda ($env k29)
                                           ((cps +)
                                            (get-cell
                                             (env-ref $env_t44 $env count))
                                            1
                                            (make-closure
                                             (lambda ($env rv30)
                                               (set-cell!
                                                (env-ref $env_t43 $env count)
                                                rv30
                                                (app*
                                                 (env-ref $env_t43 $env k29)
                                                 (void))))
                                             (make-env
                                              $env_t43
                                              (count
                                               (env-ref $env_t44 $env count))
                                              (k29 k29)))))
                                         (make-env
                                          $env_t44
                                          (count
                                           (env-ref $env_t45 $env count))))
                                        (env-ref $env_t45 $env k27))
                                       (app*
                                        (env-ref $env_t45 $env k27)
                                        (void))))
                                   (make-env
                                    $env_t45
                                    (count (env-ref $env_t46 $env count))
                                    (k27 k27)))))
                               (make-env
                                $env_t46
                                (ch (env-ref $env_t47 $env ch))
                                (count (env-ref $env_t47 $env count))))
                              k26)))
                          (make-env
                           $env_t47
                           (ch (env-ref $env_t46 $env ch))
                           (count (env-ref $env_t46 $env count))
                           (i14 i14)))
                         k25))
                      (make-env
                       $env_t46
                       (ch (env-ref $env_t48 $env ch))
                       (count (env-ref $env_t48 $env count))))
                     k17))
                  (make-env
                   $env_t48
                   (ch ch)
                   (count count)
                   (str (env-ref $env_t52 $env str))))
                 (make-closure
                  (lambda ($env rv31)
                    ((cps equal?)
                     (get-cell (env-ref $env_t51 $env count))
                     2
                     (make-closure
                      (lambda ($env rv32)
                        (if rv32
                          (app*
                           (make-closure
                            (lambda ($env k33)
                              (app* (env-ref $env_t49 $env return) #t k33))
                            (make-env
                             $env_t49
                             (return (env-ref $env_t50 $env return))))
                           (env-ref $env_t50 $env k16))
                          (app*
                           (make-closure
                            (lambda ($env k34)
                              (app* (env-ref $env_t49 $env return) #f k34))
                            (make-env
                             $env_t49
                             (return (env-ref $env_t50 $env return))))
                           (env-ref $env_t50 $env k16))))
                      (make-env
                       $env_t50
                       (k16 (env-ref $env_t51 $env k16))
                       (return (env-ref $env_t51 $env return))))))
                  (make-env
                   $env_t51
                   (count count)
                   (k16 k16)
                   (return (env-ref $env_t52 $env return)))))))))
           (make-env
            $env_t52
            (return return)
            (str (env-ref $env_t53 $env str))))
          (void)
          (void)
          k15))
       (make-env $env_t53 (str str)))
      k14))
   (make-env $env_t39))
  (app* $halt (void))))
