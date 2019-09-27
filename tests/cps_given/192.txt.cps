(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$sum (void))
 (set-then!
  g$x
  0
  (set-then!
   g$sum
   0
   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
    (lambda (break k14)
      ((lambda (loop k15)
         (set-then!
          loop
          (lambda (k16)
            ((cps <=)
             g$x
             20
             (lambda (rv17)
               (if rv17
                 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                  (lambda (continue k18)
                    ((lambda (k19)
                       ((lambda (k20)
                          ((cps modulo)
                           g$x
                           2
                           (lambda (rv23)
                             ((cps equal?)
                              rv23
                              0
                              (lambda (rv24)
                                (if rv24
                                  ((lambda (k25)
                                     ((cps +)
                                      g$sum
                                      g$x
                                      (lambda (rv26)
                                        (set-then! g$sum rv26 (k25 (void))))))
                                   k20)
                                  ((lambda (k27)
                                     ((cps +)
                                      g$x
                                      1
                                      (lambda (rv28)
                                        (set-then! g$x rv28 (break k27)))))
                                   k20)))))))
                        (lambda (rv21)
                          ((cps +)
                           g$x
                           1
                           (lambda (rv22)
                             (set-then! g$x rv22 (k19 (void))))))))
                     k18))
                  (lambda (rv29) (loop k16)))
                 (k16 (void))))))
          (loop
           (lambda (rv30)
             ((lambda (k31) ((cps py-print) "didn't run\n" k31)) k15)))))
       (void)
       k14))
    (lambda (rv32) ((cps py-print) g$sum $halt))))))
