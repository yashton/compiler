(program
 (define-env $env_t52 (return))
 (define-env $env_t53 (k30 return))
 (define-env $env_t54 (i nums return))
 (define-env $env_t38 (cc))
 (define-env $env_t39 ())
 (define-env $env_t56 (i k17 nums return))
 (define-env $env_t57 (k16 return))
 (define-env $env_t51 (i nums))
 (define-env $env_t59 (nums))
 (define-env $env_t43 (e16 i15 k35))
 (define-env $env_t44 (e16))
 (define-env $env_t45 (i))
 (define-env $env_t55 (i i14 nums return))
 (define-env $env_t58 (nums return))
 (define-env $env_t41 ($loop15 $seq14))
 (define-env $env_t46 (e18 i17 k42))
 (define-env $env_t47 (e18))
 (define-env $env_t48 (e18 k41))
 (define-env $env_t40 ($loop15 $seq14 k19))
 (define-env $env_t49 (k31))
 (define-env $env_t50 (i k31 nums))
 (define-env $env_t42 (k18))
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$pair_13 (void))
 (set-then!
  g$pair_13
  (make-closure
   (lambda ($env nums k14)
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
           (lambda ($env i k16)
             (set-then!
              i
              (make-cell i)
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
                   g$len
                   (env-ref $env_t54 $env nums)
                   (make-closure
                    (lambda ($env rv25)
                      ((cps -)
                       rv25
                       1
                       (make-closure
                        (lambda ($env rv26)
                          (app*
                           g$range
                           rv26
                           (make-closure
                            (lambda ($env rv27)
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
                                                   (env-ref
                                                    $env_t40
                                                    $env
                                                    $seq14)
                                                   (env-ref
                                                    $env_t40
                                                    $env
                                                    $loop15)
                                                   (env-ref $env_t40 $env k19))
                                                  ((cps py-list?)
                                                   (env-ref
                                                    $env_t40
                                                    $env
                                                    $seq14)
                                                   (make-closure
                                                    (lambda ($env rv23)
                                                      (if rv23
                                                        (app*
                                                         for-py-list-k
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
                                                        ((cps dict?)
                                                         (env-ref
                                                          $env_t40
                                                          $env
                                                          $seq14)
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
                                                (env-ref
                                                 $env_t40
                                                 $env
                                                 $loop15))
                                               ($seq14
                                                (env-ref $env_t40 $env $seq14))
                                               (k19
                                                (env-ref
                                                 $env_t40
                                                 $env
                                                 k19)))))))
                                        (make-env
                                         $env_t40
                                         ($loop15
                                          (env-ref $env_t41 $env $loop15))
                                         ($seq14
                                          (env-ref $env_t41 $env $seq14))
                                         (k19 k19)))))
                                    (make-env
                                     $env_t41
                                     ($loop15 $loop15)
                                     ($seq14 $seq14)))
                                   (make-closure
                                    (lambda ($env rv20)
                                      (app*
                                       (env-ref $env_t42 $env k18)
                                       (void)))
                                    (make-env $env_t42 (k18 k18)))))
                                (make-env $env_t39))
                               rv27
                               (make-closure
                                (lambda ($env i14 k28)
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
                                    (lambda ($env continue k29)
                                      (set-cell!
                                       (env-ref $env_t55 $env i)
                                       (env-ref $env_t55 $env i14)
                                       (app*
                                        (make-closure
                                         (lambda ($env k30)
                                           (app*
                                            (make-closure
                                             (lambda ($env k31)
                                               (app*
                                                (make-closure
                                                 (lambda ($env e16 k34)
                                                   (app*
                                                    (make-closure
                                                     (lambda ($env i15 k35)
                                                       ((cps py-list?)
                                                        (env-ref
                                                         $env_t44
                                                         $env
                                                         e16)
                                                        (make-closure
                                                         (lambda ($env rv36)
                                                           (if rv36
                                                             ((cps py-list-ref)
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               e16)
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               i15)
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               k35))
                                                             ((cps tuple?)
                                                              (env-ref
                                                               $env_t43
                                                               $env
                                                               e16)
                                                              (make-closure
                                                               (lambda ($env
                                                                        rv37)
                                                                 (if rv37
                                                                   ((cps
                                                                     tuple-ref)
                                                                    (env-ref
                                                                     $env_t43
                                                                     $env
                                                                     e16)
                                                                    (env-ref
                                                                     $env_t43
                                                                     $env
                                                                     i15)
                                                                    (env-ref
                                                                     $env_t43
                                                                     $env
                                                                     k35))
                                                                   ((cps dict?)
                                                                    (env-ref
                                                                     $env_t43
                                                                     $env
                                                                     e16)
                                                                    (make-closure
                                                                     (lambda ($env
                                                                              rv38)
                                                                       (if rv38
                                                                         ((cps
                                                                           dict-ref)
                                                                          (env-ref
                                                                           $env_t43
                                                                           $env
                                                                           e16)
                                                                          (env-ref
                                                                           $env_t43
                                                                           $env
                                                                           i15)
                                                                          (env-ref
                                                                           $env_t43
                                                                           $env
                                                                           k35))
                                                                         (error
                                                                          "cannot index object"
                                                                          (env-ref
                                                                           $env_t43
                                                                           $env
                                                                           k35))))
                                                                     (make-env
                                                                      $env_t43
                                                                      (e16
                                                                       (env-ref
                                                                        $env_t43
                                                                        $env
                                                                        e16))
                                                                      (i15
                                                                       (env-ref
                                                                        $env_t43
                                                                        $env
                                                                        i15))
                                                                      (k35
                                                                       (env-ref
                                                                        $env_t43
                                                                        $env
                                                                        k35)))))))
                                                               (make-env
                                                                $env_t43
                                                                (e16
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  e16))
                                                                (i15
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  i15))
                                                                (k35
                                                                 (env-ref
                                                                  $env_t43
                                                                  $env
                                                                  k35)))))))
                                                         (make-env
                                                          $env_t43
                                                          (e16
                                                           (env-ref
                                                            $env_t44
                                                            $env
                                                            e16))
                                                          (i15 i15)
                                                          (k35 k35)))))
                                                     (make-env
                                                      $env_t44
                                                      (e16 e16)))
                                                    (get-cell
                                                     (env-ref $env_t45 $env i))
                                                    k34))
                                                 (make-env
                                                  $env_t45
                                                  (i
                                                   (env-ref $env_t51 $env i))))
                                                (env-ref $env_t51 $env nums)
                                                (make-closure
                                                 (lambda ($env rv39)
                                                   ((cps equal?)
                                                    rv39
                                                    13
                                                    (make-closure
                                                     (lambda ($env rv40)
                                                       (if rv40
                                                         (app*
                                                          (make-closure
                                                           (lambda ($env
                                                                    e18
                                                                    k41)
                                                             ((cps +)
                                                              (get-cell
                                                               (env-ref
                                                                $env_t45
                                                                $env
                                                                i))
                                                              1
                                                              (make-closure
                                                               (lambda ($env
                                                                        rv46)
                                                                 (app*
                                                                  (make-closure
                                                                   (lambda ($env
                                                                            i17
                                                                            k42)
                                                                     ((cps
                                                                       py-list?)
                                                                      (env-ref
                                                                       $env_t47
                                                                       $env
                                                                       e18)
                                                                      (make-closure
                                                                       (lambda ($env
                                                                                rv43)
                                                                         (if rv43
                                                                           ((cps
                                                                             py-list-ref)
                                                                            (env-ref
                                                                             $env_t46
                                                                             $env
                                                                             e18)
                                                                            (env-ref
                                                                             $env_t46
                                                                             $env
                                                                             i17)
                                                                            (env-ref
                                                                             $env_t46
                                                                             $env
                                                                             k42))
                                                                           ((cps
                                                                             tuple?)
                                                                            (env-ref
                                                                             $env_t46
                                                                             $env
                                                                             e18)
                                                                            (make-closure
                                                                             (lambda ($env
                                                                                      rv44)
                                                                               (if rv44
                                                                                 ((cps
                                                                                   tuple-ref)
                                                                                  (env-ref
                                                                                   $env_t46
                                                                                   $env
                                                                                   e18)
                                                                                  (env-ref
                                                                                   $env_t46
                                                                                   $env
                                                                                   i17)
                                                                                  (env-ref
                                                                                   $env_t46
                                                                                   $env
                                                                                   k42))
                                                                                 ((cps
                                                                                   dict?)
                                                                                  (env-ref
                                                                                   $env_t46
                                                                                   $env
                                                                                   e18)
                                                                                  (make-closure
                                                                                   (lambda ($env
                                                                                            rv45)
                                                                                     (if rv45
                                                                                       ((cps
                                                                                         dict-ref)
                                                                                        (env-ref
                                                                                         $env_t46
                                                                                         $env
                                                                                         e18)
                                                                                        (env-ref
                                                                                         $env_t46
                                                                                         $env
                                                                                         i17)
                                                                                        (env-ref
                                                                                         $env_t46
                                                                                         $env
                                                                                         k42))
                                                                                       (error
                                                                                        "cannot index object"
                                                                                        (env-ref
                                                                                         $env_t46
                                                                                         $env
                                                                                         k42))))
                                                                                   (make-env
                                                                                    $env_t46
                                                                                    (e18
                                                                                     (env-ref
                                                                                      $env_t46
                                                                                      $env
                                                                                      e18))
                                                                                    (i17
                                                                                     (env-ref
                                                                                      $env_t46
                                                                                      $env
                                                                                      i17))
                                                                                    (k42
                                                                                     (env-ref
                                                                                      $env_t46
                                                                                      $env
                                                                                      k42)))))))
                                                                             (make-env
                                                                              $env_t46
                                                                              (e18
                                                                               (env-ref
                                                                                $env_t46
                                                                                $env
                                                                                e18))
                                                                              (i17
                                                                               (env-ref
                                                                                $env_t46
                                                                                $env
                                                                                i17))
                                                                              (k42
                                                                               (env-ref
                                                                                $env_t46
                                                                                $env
                                                                                k42)))))))
                                                                       (make-env
                                                                        $env_t46
                                                                        (e18
                                                                         (env-ref
                                                                          $env_t47
                                                                          $env
                                                                          e18))
                                                                        (i17
                                                                         i17)
                                                                        (k42
                                                                         k42)))))
                                                                   (make-env
                                                                    $env_t47
                                                                    (e18
                                                                     (env-ref
                                                                      $env_t48
                                                                      $env
                                                                      e18))))
                                                                  rv46
                                                                  (env-ref
                                                                   $env_t48
                                                                   $env
                                                                   k41)))
                                                               (make-env
                                                                $env_t48
                                                                (e18 e18)
                                                                (k41 k41)))))
                                                           (make-env
                                                            $env_t45
                                                            (i
                                                             (env-ref
                                                              $env_t50
                                                              $env
                                                              i))))
                                                          (env-ref
                                                           $env_t50
                                                           $env
                                                           nums)
                                                          (make-closure
                                                           (lambda ($env rv47)
                                                             ((cps equal?)
                                                              rv47
                                                              13
                                                              (env-ref
                                                               $env_t49
                                                               $env
                                                               k31)))
                                                           (make-env
                                                            $env_t49
                                                            (k31
                                                             (env-ref
                                                              $env_t50
                                                              $env
                                                              k31)))))
                                                         (app*
                                                          (env-ref
                                                           $env_t50
                                                           $env
                                                           k31)
                                                          #f)))
                                                     (make-env
                                                      $env_t50
                                                      (i
                                                       (env-ref
                                                        $env_t50
                                                        $env
                                                        i))
                                                      (k31
                                                       (env-ref
                                                        $env_t50
                                                        $env
                                                        k31))
                                                      (nums
                                                       (env-ref
                                                        $env_t50
                                                        $env
                                                        nums))))))
                                                 (make-env
                                                  $env_t50
                                                  (i (env-ref $env_t51 $env i))
                                                  (k31 k31)
                                                  (nums
                                                   (env-ref
                                                    $env_t51
                                                    $env
                                                    nums))))))
                                             (make-env
                                              $env_t51
                                              (i (env-ref $env_t54 $env i))
                                              (nums
                                               (env-ref $env_t54 $env nums))))
                                            (make-closure
                                             (lambda ($env rv32)
                                               (if rv32
                                                 (app*
                                                  (make-closure
                                                   (lambda ($env k33)
                                                     (app*
                                                      (env-ref
                                                       $env_t52
                                                       $env
                                                       return)
                                                      #t
                                                      k33))
                                                   (make-env
                                                    $env_t52
                                                    (return
                                                     (env-ref
                                                      $env_t53
                                                      $env
                                                      return))))
                                                  (env-ref $env_t53 $env k30))
                                                 (app*
                                                  (env-ref $env_t53 $env k30)
                                                  (void))))
                                             (make-env
                                              $env_t53
                                              (k30 k30)
                                              (return
                                               (env-ref
                                                $env_t54
                                                $env
                                                return))))))
                                         (make-env
                                          $env_t54
                                          (i (env-ref $env_t55 $env i))
                                          (nums (env-ref $env_t55 $env nums))
                                          (return
                                           (env-ref $env_t55 $env return))))
                                        k29)))
                                    (make-env
                                     $env_t55
                                     (i (env-ref $env_t54 $env i))
                                     (i14 i14)
                                     (nums (env-ref $env_t54 $env nums))
                                     (return (env-ref $env_t54 $env return))))
                                   k28))
                                (make-env
                                 $env_t54
                                 (i (env-ref $env_t56 $env i))
                                 (nums (env-ref $env_t56 $env nums))
                                 (return (env-ref $env_t56 $env return))))
                               (env-ref $env_t56 $env k17)))
                            (make-env
                             $env_t56
                             (i (env-ref $env_t56 $env i))
                             (k17 (env-ref $env_t56 $env k17))
                             (nums (env-ref $env_t56 $env nums))
                             (return (env-ref $env_t56 $env return))))))
                        (make-env
                         $env_t56
                         (i (env-ref $env_t56 $env i))
                         (k17 (env-ref $env_t56 $env k17))
                         (nums (env-ref $env_t56 $env nums))
                         (return (env-ref $env_t56 $env return))))))
                    (make-env
                     $env_t56
                     (i (env-ref $env_t54 $env i))
                     (k17 k17)
                     (nums (env-ref $env_t54 $env nums))
                     (return (env-ref $env_t54 $env return))))))
                (make-env
                 $env_t54
                 (i i)
                 (nums (env-ref $env_t58 $env nums))
                 (return (env-ref $env_t58 $env return))))
               (make-closure
                (lambda ($env rv48)
                  (app*
                   (env-ref $env_t57 $env return)
                   #f
                   (env-ref $env_t57 $env k16)))
                (make-env
                 $env_t57
                 (k16 k16)
                 (return (env-ref $env_t58 $env return)))))))
           (make-env
            $env_t58
            (nums (env-ref $env_t59 $env nums))
            (return return)))
          (void)
          k15))
       (make-env $env_t59 (nums nums)))
      k14))
   (make-env $env_t39))
  (app* $halt (void))))
