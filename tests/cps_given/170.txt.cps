(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$x (void))
 (set-then!
  g$x
  10
  (set-then!
   g$f
   (lambda (k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15)
        ((lambda (x g k16)
           (set-then!
            x
            20
            (set-then!
             g
             (lambda (k17)
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (return k18)
                  ((lambda (k19) (set-then! g$x 30 (k19 (void)))) k18))
                k17))
             (g k16))))
         (void)
         (void)
         k15))
      k14))
   (g$f (lambda (rv20) ((cps py-print) g$x $halt))))))
