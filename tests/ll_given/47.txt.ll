(program
 (define-label
  $lambda38
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda39
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda38) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda40
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda41
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda40) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda42
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda43
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda42) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda44
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda45
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda44) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda46
  (lambda ($env x k) (app* (env-ref $env_t38 $env cc) x)))
 (define-label
  $lambda47
  (lambda ($env f cc)
    (app*
     f
     (make-closure (lambda-label $lambda46) (make-env $env_t38 (cc cc)))
     cc)))
 (define-label
  $lambda48
  (lambda ($env rv30)
    (set-cell!
     (env-ref $env_t40 $env j)
     rv30
     (app* (env-ref $env_t40 $env k28) (void)))))
 (define-label
  $lambda49
  (lambda ($env rv29)
    (set-cell!
     (env-ref $env_t41 $env prod)
     rv29
     ((cps +)
      (get-cell (env-ref $env_t41 $env j))
      1
      (make-closure
       (lambda-label $lambda48)
       (make-env
        $env_t40
        (j (env-ref $env_t41 $env j))
        (k28 (env-ref $env_t41 $env k28))))))))
 (define-label
  $lambda50
  (lambda ($env k28)
    ((cps *)
     (get-cell (env-ref $env_t42 $env prod))
     (env-ref $env_t42 $env x)
     (make-closure
      (lambda-label $lambda49)
      (make-env
       $env_t41
       (j (env-ref $env_t42 $env j))
       (k28 k28)
       (prod (env-ref $env_t42 $env prod)))))))
 (define-label
  $lambda51
  (lambda ($env continue k27)
    (app*
     (make-closure
      (lambda-label $lambda50)
      (make-env
       $env_t42
       (j (env-ref $env_t42 $env j))
       (prod (env-ref $env_t42 $env prod))
       (x (env-ref $env_t42 $env x))))
     k27)))
 (define-label
  $lambda52
  (lambda ($env rv31)
    (app*
     (get-cell (env-ref $env_t43 $env loop))
     (env-ref $env_t43 $env k25))))
 (define-label
  $lambda53
  (lambda ($env rv26)
    (if rv26
      (app*
       (make-closure (lambda-label $lambda47) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda51)
        (make-env
         $env_t42
         (j (env-ref $env_t44 $env j))
         (prod (env-ref $env_t44 $env prod))
         (x (env-ref $env_t44 $env x))))
       (make-closure
        (lambda-label $lambda52)
        (make-env
         $env_t43
         (k25 (env-ref $env_t44 $env k25))
         (loop (env-ref $env_t44 $env loop)))))
      (app* (env-ref $env_t44 $env k25) (void)))))
 (define-label
  $lambda54
  (lambda ($env k25)
    ((cps <)
     (get-cell (env-ref $env_t45 $env j))
     (get-cell (env-ref $env_t45 $env i))
     (make-closure
      (lambda-label $lambda53)
      (make-env
       $env_t44
       (j (env-ref $env_t45 $env j))
       (k25 k25)
       (loop (env-ref $env_t45 $env loop))
       (prod (env-ref $env_t45 $env prod))
       (x (env-ref $env_t45 $env x)))))))
 (define-label
  $lambda55
  (lambda ($env rv32) (app* (env-ref $env_t46 $env k24) (void))))
 (define-label
  $lambda56
  (lambda ($env loop k24)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure
       (lambda-label $lambda54)
       (make-env
        $env_t45
        (i (env-ref $env_t47 $env i))
        (j (env-ref $env_t47 $env j))
        (loop loop)
        (prod (env-ref $env_t47 $env prod))
        (x (env-ref $env_t47 $env x))))
      (app*
       (get-cell loop)
       (make-closure
        (lambda-label $lambda55)
        (make-env $env_t46 (k24 k24))))))))
 (define-label
  $lambda57
  (lambda ($env break k23)
    (app*
     (make-closure
      (lambda-label $lambda56)
      (make-env
       $env_t47
       (i (env-ref $env_t47 $env i))
       (j (env-ref $env_t47 $env j))
       (prod (env-ref $env_t47 $env prod))
       (x (env-ref $env_t47 $env x))))
     (void)
     k23)))
 (define-label
  $lambda58
  (lambda ($env rv35)
    (set-cell!
     (env-ref $env_t48 $env i)
     rv35
     (app* (env-ref $env_t48 $env k22) (void)))))
 (define-label
  $lambda59
  (lambda ($env rv34)
    (set-cell!
     (env-ref $env_t49 $env sum)
     rv34
     ((cps +)
      (get-cell (env-ref $env_t49 $env i))
      1
      (make-closure
       (lambda-label $lambda58)
       (make-env
        $env_t48
        (i (env-ref $env_t49 $env i))
        (k22 (env-ref $env_t49 $env k22))))))))
 (define-label
  $lambda60
  (lambda ($env rv33)
    ((cps +)
     (get-cell (env-ref $env_t50 $env sum))
     (get-cell (env-ref $env_t50 $env prod))
     (make-closure
      (lambda-label $lambda59)
      (make-env
       $env_t49
       (i (env-ref $env_t50 $env i))
       (k22 (env-ref $env_t50 $env k22))
       (sum (env-ref $env_t50 $env sum)))))))
 (define-label
  $lambda61
  (lambda ($env k22)
    (set-cell!
     (env-ref $env_t51 $env prod)
     1
     (set-cell!
      (env-ref $env_t51 $env j)
      0
      (app*
       (make-closure (lambda-label $lambda45) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda57)
        (make-env
         $env_t47
         (i (env-ref $env_t51 $env i))
         (j (env-ref $env_t51 $env j))
         (prod (env-ref $env_t51 $env prod))
         (x (env-ref $env_t51 $env x))))
       (make-closure
        (lambda-label $lambda60)
        (make-env
         $env_t50
         (i (env-ref $env_t51 $env i))
         (k22 k22)
         (prod (env-ref $env_t51 $env prod))
         (sum (env-ref $env_t51 $env sum)))))))))
 (define-label
  $lambda62
  (lambda ($env continue k21)
    (app*
     (make-closure
      (lambda-label $lambda61)
      (make-env
       $env_t51
       (i (env-ref $env_t51 $env i))
       (j (env-ref $env_t51 $env j))
       (prod (env-ref $env_t51 $env prod))
       (sum (env-ref $env_t51 $env sum))
       (x (env-ref $env_t51 $env x))))
     k21)))
 (define-label
  $lambda63
  (lambda ($env rv36)
    (app*
     (get-cell (env-ref $env_t52 $env loop))
     (env-ref $env_t52 $env k19))))
 (define-label
  $lambda64
  (lambda ($env rv20)
    (if rv20
      (app*
       (make-closure (lambda-label $lambda43) (make-env $env_t39))
       (make-closure
        (lambda-label $lambda62)
        (make-env
         $env_t51
         (i (env-ref $env_t53 $env i))
         (j (env-ref $env_t53 $env j))
         (prod (env-ref $env_t53 $env prod))
         (sum (env-ref $env_t53 $env sum))
         (x (env-ref $env_t53 $env x))))
       (make-closure
        (lambda-label $lambda63)
        (make-env
         $env_t52
         (k19 (env-ref $env_t53 $env k19))
         (loop (env-ref $env_t53 $env loop)))))
      (app* (env-ref $env_t53 $env k19) (void)))))
 (define-label
  $lambda65
  (lambda ($env k19)
    ((cps <=)
     (get-cell (env-ref $env_t54 $env i))
     (env-ref $env_t54 $env n)
     (make-closure
      (lambda-label $lambda64)
      (make-env
       $env_t53
       (i (env-ref $env_t54 $env i))
       (j (env-ref $env_t54 $env j))
       (k19 k19)
       (loop (env-ref $env_t54 $env loop))
       (prod (env-ref $env_t54 $env prod))
       (sum (env-ref $env_t54 $env sum))
       (x (env-ref $env_t54 $env x)))))))
 (define-label
  $lambda66
  (lambda ($env rv37) (app* (env-ref $env_t55 $env k18) (void))))
 (define-label
  $lambda67
  (lambda ($env loop k18)
    (set-then!
     loop
     (make-cell loop)
     (set-cell!
      loop
      (make-closure
       (lambda-label $lambda65)
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
        (lambda-label $lambda66)
        (make-env $env_t55 (k18 k18))))))))
 (define-label
  $lambda68
  (lambda ($env break k17)
    (app*
     (make-closure
      (lambda-label $lambda67)
      (make-env
       $env_t56
       (i (env-ref $env_t56 $env i))
       (j (env-ref $env_t56 $env j))
       (n (env-ref $env_t56 $env n))
       (prod (env-ref $env_t56 $env prod))
       (sum (env-ref $env_t56 $env sum))
       (x (env-ref $env_t56 $env x))))
     (void)
     k17)))
 (define-label
  $lambda69
  (lambda ($env rv38)
    (app*
     (env-ref $env_t57 $env return)
     (get-cell (env-ref $env_t57 $env sum))
     (env-ref $env_t57 $env k16))))
 (define-label
  $lambda70
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
           (make-closure (lambda-label $lambda41) (make-env $env_t39))
           (make-closure
            (lambda-label $lambda68)
            (make-env
             $env_t56
             (i i)
             (j j)
             (n (env-ref $env_t58 $env n))
             (prod prod)
             (sum sum)
             (x (env-ref $env_t58 $env x))))
           (make-closure
            (lambda-label $lambda69)
            (make-env
             $env_t57
             (k16 k16)
             (return (env-ref $env_t58 $env return))
             (sum sum))))))))))))
 (define-label
  $lambda71
  (lambda ($env return k15)
    (app*
     (make-closure
      (lambda-label $lambda70)
      (make-env
       $env_t58
       (n (env-ref $env_t59 $env n))
       (return return)
       (x (env-ref $env_t59 $env x))))
     (void)
     (void)
     (void)
     (void)
     k15)))
 (define-label
  $lambda72
  (lambda ($env x n k14)
    (app*
     (make-closure (lambda-label $lambda39) (make-env $env_t39))
     (make-closure (lambda-label $lambda71) (make-env $env_t59 (n n) (x x)))
     k14)))
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
  (make-closure (lambda-label $lambda72) (make-env $env_t39))
  (app* $halt (void))))
