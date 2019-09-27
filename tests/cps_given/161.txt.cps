(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  ((cps py-print)
   g$x
   (lambda (rv14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (break k15)
        ((lambda (loop k16)
           (set-then!
            loop
            (lambda (k17)
              ((cps >)
               g$x
               0
               (lambda (rv18)
                 (if rv18
                   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                    (lambda (continue k19)
                      ((lambda (k20)
                         ((cps py-print)
                          g$x
                          (lambda (rv21)
                            ((cps -)
                             g$x
                             1
                             (lambda (rv22)
                               (set-then!
                                g$x
                                rv22
                                ((cps py-print) g$x k20)))))))
                       k19))
                    (lambda (rv23) (loop k17)))
                   (k17 (void))))))
            (loop
             (lambda (rv24)
               ((lambda (k25) ((cps py-print) "didn't run\n" k25)) k16)))))
         (void)
         k15))
      $halt)))))
