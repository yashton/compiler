(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k14)
     ((lambda (loop k15)
        (set-then!
         loop
         (lambda (k16)
           ((cps >)
            g$x
            0
            (lambda (rv17)
              (if rv17
                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                 (lambda (continue k18)
                   ((lambda (k19)
                      ((cps -)
                       g$x
                       1
                       (lambda (rv20) (set-then! g$x rv20 (k19 (void))))))
                    k18))
                 (lambda (rv21) (loop k16)))
                (k16 (void))))))
         (loop
          (lambda (rv22)
            ((lambda (k23) ((cps py-print) "didn't run\n" k23)) k15)))))
      (void)
      k14))
   $halt)))
