(program
 (define-env $env_t43 (k25 loop))
 (define-env $env_t44 (j k25 loop prod x))
 (define-env $env_t45 (i j loop prod x))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t46 (k24))
 (define-env $env_t47 (i j prod x))
 (define-env $env_t48 (i k22))
 (define-env $env_t49 (i k22 sum))
 (define-env $env_t50 (i k22 prod sum))
 (define-env $env_t51 (i j prod sum x))
 (define-env $env_t52 (k19 loop))
 (define-env $env_t53 (i j k19 loop prod sum x))
 (define-env $env_t54 (i j loop n prod sum x))
 (define-env $env_t55 (k18))
 (define-env $env_t56 (i j n prod sum x))
 (define-env $env_t57 (k16 return sum))
 (define-env $env_t58 (n return x))
 (define-env $env_t59 (n x))
 (define-env $env_t40 (j k28))
 (define-env $env_t41 (j k28 prod))
 (define-env $env_t42 (j prod x))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$geometricSeriesSum (void))
 (set-then!
  g$geometricSeriesSum
  (make-closure
   (lambda ($env x n k14)
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
           (lambda ($env sum i prod j k16)
             (set-then!
              sum
              (make-cell sum)
              (set-then!
               i
               (make-cell i)
               (set-then!
                prod
                (make-cell prod)
                (set-then!
                 j
                 (make-cell j)
                 (set-cell!
                  sum
                  0
                  (set-cell!
                   i
                   0
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
                     (lambda ($env break k17)
                       (app*
                        (make-closure
                         (lambda ($env loop k18)
                           (set-then!
                            loop
                            (make-cell loop)
                            (set-cell!
                             loop
                             (make-closure
                              (lambda ($env k19)
                                ((cps <=)
                                 (get-cell (env-ref $env_t54 $env i))
                                 (env-ref $env_t54 $env n)
                                 (make-closure
                                  (lambda ($env rv20)
                                    (if rv20
                                      (app*
                                       (make-closure
                                        (lambda ($env f cc)
                                          (app*
                                           f
                                           (make-closure
                                            (lambda ($env x k)
                                              (app*
                                               (env-ref $env_t38 $env cc)
                                               x))
                                            (make-env $env_t38 (cc cc)))
                                           cc))
                                        (make-env $env_t39))
                                       (make-closure
                                        (lambda ($env continue k21)
                                          (app*
                                           (make-closure
                                            (lambda ($env k22)
                                              (set-cell!
                                               (env-ref $env_t51 $env prod)
                                               1
                                               (set-cell!
                                                (env-ref $env_t51 $env j)
                                                0
                                                (app*
                                                 (make-closure
                                                  (lambda ($env f cc)
                                                    (app*
                                                     f
                                                     (make-closure
                                                      (lambda ($env x k)
                                                        (app*
                                                         (env-ref
                                                          $env_t38
                                                          $env
                                                          cc)
                                                         x))
                                                      (make-env
                                                       $env_t38
                                                       (cc cc)))
                                                     cc))
                                                  (make-env $env_t39))
                                                 (make-closure
                                                  (lambda ($env break k23)
                                                    (app*
                                                     (make-closure
                                                      (lambda ($env loop k24)
                                                        (set-then!
                                                         loop
                                                         (make-cell loop)
                                                         (set-cell!
                                                          loop
                                                          (make-closure
                                                           (lambda ($env k25)
                                                             ((cps <)
                                                              (get-cell
                                                               (env-ref
                                                                $env_t45
                                                                $env
                                                                j))
                                                              (get-cell
                                                               (env-ref
                                                                $env_t45
                                                                $env
                                                                i))
                                                              (make-closure
                                                               (lambda ($env
                                                                        rv26)
                                                                 (if rv26
                                                                   (app*
                                                                    (make-closure
                                                                     (lambda ($env
                                                                              f
                                                                              cc)
                                                                       (app*
                                                                        f
                                                                        (make-closure
                                                                         (lambda ($env
                                                                                  x
                                                                                  k)
                                                                           (app*
                                                                            (env-ref
                                                                             $env_t38
                                                                             $env
                                                                             cc)
                                                                            x))
                                                                         (make-env
                                                                          $env_t38
                                                                          (cc
                                                                           cc)))
                                                                        cc))
                                                                     (make-env
                                                                      $env_t39))
                                                                    (make-closure
                                                                     (lambda ($env
                                                                              continue
                                                                              k27)
                                                                       (app*
                                                                        (make-closure
                                                                         (lambda ($env
                                                                                  k28)
                                                                           ((cps
                                                                             *)
                                                                            (get-cell
                                                                             (env-ref
                                                                              $env_t42
                                                                              $env
                                                                              prod))
                                                                            (env-ref
                                                                             $env_t42
                                                                             $env
                                                                             x)
                                                                            (make-closure
                                                                             (lambda ($env
                                                                                      rv29)
                                                                               (set-cell!
                                                                                (env-ref
                                                                                 $env_t41
                                                                                 $env
                                                                                 prod)
                                                                                rv29
                                                                                ((cps
                                                                                  +)
                                                                                 (get-cell
                                                                                  (env-ref
                                                                                   $env_t41
                                                                                   $env
                                                                                   j))
                                                                                 1
                                                                                 (make-closure
                                                                                  (lambda ($env
                                                                                           rv30)
                                                                                    (set-cell!
                                                                                     (env-ref
                                                                                      $env_t40
                                                                                      $env
                                                                                      j)
                                                                                     rv30
                                                                                     (app*
                                                                                      (env-ref
                                                                                       $env_t40
                                                                                       $env
                                                                                       k28)
                                                                                      (void))))
                                                                                  (make-env
                                                                                   $env_t40
                                                                                   (j
                                                                                    (env-ref
                                                                                     $env_t41
                                                                                     $env
                                                                                     j))
                                                                                   (k28
                                                                                    (env-ref
                                                                                     $env_t41
                                                                                     $env
                                                                                     k28)))))))
                                                                             (make-env
                                                                              $env_t41
                                                                              (j
                                                                               (env-ref
                                                                                $env_t42
                                                                                $env
                                                                                j))
                                                                              (k28
                                                                               k28)
                                                                              (prod
                                                                               (env-ref
                                                                                $env_t42
                                                                                $env
                                                                                prod))))))
                                                                         (make-env
                                                                          $env_t42
                                                                          (j
                                                                           (env-ref
                                                                            $env_t42
                                                                            $env
                                                                            j))
                                                                          (prod
                                                                           (env-ref
                                                                            $env_t42
                                                                            $env
                                                                            prod))
                                                                          (x
                                                                           (env-ref
                                                                            $env_t42
                                                                            $env
                                                                            x))))
                                                                        k27))
                                                                     (make-env
                                                                      $env_t42
                                                                      (j
                                                                       (env-ref
                                                                        $env_t44
                                                                        $env
                                                                        j))
                                                                      (prod
                                                                       (env-ref
                                                                        $env_t44
                                                                        $env
                                                                        prod))
                                                                      (x
                                                                       (env-ref
                                                                        $env_t44
                                                                        $env
                                                                        x))))
                                                                    (make-closure
                                                                     (lambda ($env
                                                                              rv31)
                                                                       (app*
                                                                        (get-cell
                                                                         (env-ref
                                                                          $env_t43
                                                                          $env
                                                                          loop))
                                                                        (env-ref
                                                                         $env_t43
                                                                         $env
                                                                         k25)))
                                                                     (make-env
                                                                      $env_t43
                                                                      (k25
                                                                       (env-ref
                                                                        $env_t44
                                                                        $env
                                                                        k25))
                                                                      (loop
                                                                       (env-ref
                                                                        $env_t44
                                                                        $env
                                                                        loop)))))
                                                                   (app*
                                                                    (env-ref
                                                                     $env_t44
                                                                     $env
                                                                     k25)
                                                                    (void))))
                                                               (make-env
                                                                $env_t44
                                                                (j
                                                                 (env-ref
                                                                  $env_t45
                                                                  $env
                                                                  j))
                                                                (k25 k25)
                                                                (loop
                                                                 (env-ref
                                                                  $env_t45
                                                                  $env
                                                                  loop))
                                                                (prod
                                                                 (env-ref
                                                                  $env_t45
                                                                  $env
                                                                  prod))
                                                                (x
                                                                 (env-ref
                                                                  $env_t45
                                                                  $env
                                                                  x))))))
                                                           (make-env
                                                            $env_t45
                                                            (i
                                                             (env-ref
                                                              $env_t47
                                                              $env
                                                              i))
                                                            (j
                                                             (env-ref
                                                              $env_t47
                                                              $env
                                                              j))
                                                            (loop loop)
                                                            (prod
                                                             (env-ref
                                                              $env_t47
                                                              $env
                                                              prod))
                                                            (x
                                                             (env-ref
                                                              $env_t47
                                                              $env
                                                              x))))
                                                          (app*
                                                           (get-cell loop)
                                                           (make-closure
                                                            (lambda ($env rv32)
                                                              (app*
                                                               (env-ref
                                                                $env_t46
                                                                $env
                                                                k24)
                                                               (void)))
                                                            (make-env
                                                             $env_t46
                                                             (k24 k24)))))))
                                                      (make-env
                                                       $env_t47
                                                       (i
                                                        (env-ref
                                                         $env_t47
                                                         $env
                                                         i))
                                                       (j
                                                        (env-ref
                                                         $env_t47
                                                         $env
                                                         j))
                                                       (prod
                                                        (env-ref
                                                         $env_t47
                                                         $env
                                                         prod))
                                                       (x
                                                        (env-ref
                                                         $env_t47
                                                         $env
                                                         x))))
                                                     (void)
                                                     k23))
                                                  (make-env
                                                   $env_t47
                                                   (i
                                                    (env-ref $env_t51 $env i))
                                                   (j
                                                    (env-ref $env_t51 $env j))
                                                   (prod
                                                    (env-ref
                                                     $env_t51
                                                     $env
                                                     prod))
                                                   (x
                                                    (env-ref
                                                     $env_t51
                                                     $env
                                                     x))))
                                                 (make-closure
                                                  (lambda ($env rv33)
                                                    ((cps +)
                                                     (get-cell
                                                      (env-ref
                                                       $env_t50
                                                       $env
                                                       sum))
                                                     (get-cell
                                                      (env-ref
                                                       $env_t50
                                                       $env
                                                       prod))
                                                     (make-closure
                                                      (lambda ($env rv34)
                                                        (set-cell!
                                                         (env-ref
                                                          $env_t49
                                                          $env
                                                          sum)
                                                         rv34
                                                         ((cps +)
                                                          (get-cell
                                                           (env-ref
                                                            $env_t49
                                                            $env
                                                            i))
                                                          1
                                                          (make-closure
                                                           (lambda ($env rv35)
                                                             (set-cell!
                                                              (env-ref
                                                               $env_t48
                                                               $env
                                                               i)
                                                              rv35
                                                              (app*
                                                               (env-ref
                                                                $env_t48
                                                                $env
                                                                k22)
                                                               (void))))
                                                           (make-env
                                                            $env_t48
                                                            (i
                                                             (env-ref
                                                              $env_t49
                                                              $env
                                                              i))
                                                            (k22
                                                             (env-ref
                                                              $env_t49
                                                              $env
                                                              k22)))))))
                                                      (make-env
                                                       $env_t49
                                                       (i
                                                        (env-ref
                                                         $env_t50
                                                         $env
                                                         i))
                                                       (k22
                                                        (env-ref
                                                         $env_t50
                                                         $env
                                                         k22))
                                                       (sum
                                                        (env-ref
                                                         $env_t50
                                                         $env
                                                         sum))))))
                                                  (make-env
                                                   $env_t50
                                                   (i
                                                    (env-ref $env_t51 $env i))
                                                   (k22 k22)
                                                   (prod
                                                    (env-ref
                                                     $env_t51
                                                     $env
                                                     prod))
                                                   (sum
                                                    (env-ref
                                                     $env_t51
                                                     $env
                                                     sum))))))))
                                            (make-env
                                             $env_t51
                                             (i (env-ref $env_t51 $env i))
                                             (j (env-ref $env_t51 $env j))
                                             (prod
                                              (env-ref $env_t51 $env prod))
                                             (sum (env-ref $env_t51 $env sum))
                                             (x (env-ref $env_t51 $env x))))
                                           k21))
                                        (make-env
                                         $env_t51
                                         (i (env-ref $env_t53 $env i))
                                         (j (env-ref $env_t53 $env j))
                                         (prod (env-ref $env_t53 $env prod))
                                         (sum (env-ref $env_t53 $env sum))
                                         (x (env-ref $env_t53 $env x))))
                                       (make-closure
                                        (lambda ($env rv36)
                                          (app*
                                           (get-cell
                                            (env-ref $env_t52 $env loop))
                                           (env-ref $env_t52 $env k19)))
                                        (make-env
                                         $env_t52
                                         (k19 (env-ref $env_t53 $env k19))
                                         (loop (env-ref $env_t53 $env loop)))))
                                      (app*
                                       (env-ref $env_t53 $env k19)
                                       (void))))
                                  (make-env
                                   $env_t53
                                   (i (env-ref $env_t54 $env i))
                                   (j (env-ref $env_t54 $env j))
                                   (k19 k19)
                                   (loop (env-ref $env_t54 $env loop))
                                   (prod (env-ref $env_t54 $env prod))
                                   (sum (env-ref $env_t54 $env sum))
                                   (x (env-ref $env_t54 $env x))))))
                              (make-env
                               $env_t54
                               (i (env-ref $env_t56 $env i))
                               (j (env-ref $env_t56 $env j))
                               (loop loop)
                               (n (env-ref $env_t56 $env n))
                               (prod (env-ref $env_t56 $env prod))
                               (sum (env-ref $env_t56 $env sum))
                               (x (env-ref $env_t56 $env x))))
                             (app*
                              (get-cell loop)
                              (make-closure
                               (lambda ($env rv37)
                                 (app* (env-ref $env_t55 $env k18) (void)))
                               (make-env $env_t55 (k18 k18)))))))
                         (make-env
                          $env_t56
                          (i (env-ref $env_t56 $env i))
                          (j (env-ref $env_t56 $env j))
                          (n (env-ref $env_t56 $env n))
                          (prod (env-ref $env_t56 $env prod))
                          (sum (env-ref $env_t56 $env sum))
                          (x (env-ref $env_t56 $env x))))
                        (void)
                        k17))
                     (make-env
                      $env_t56
                      (i i)
                      (j j)
                      (n (env-ref $env_t58 $env n))
                      (prod prod)
                      (sum sum)
                      (x (env-ref $env_t58 $env x))))
                    (make-closure
                     (lambda ($env rv38)
                       (app*
                        (env-ref $env_t57 $env return)
                        (get-cell (env-ref $env_t57 $env sum))
                        (env-ref $env_t57 $env k16)))
                     (make-env
                      $env_t57
                      (k16 k16)
                      (return (env-ref $env_t58 $env return))
                      (sum sum)))))))))))
           (make-env
            $env_t58
            (n (env-ref $env_t59 $env n))
            (return return)
            (x (env-ref $env_t59 $env x))))
          (void)
          (void)
          (void)
          (void)
          k15))
       (make-env $env_t59 (n n) (x x)))
      k14))
   (make-env $env_t39))
  (app* $halt (void))))
