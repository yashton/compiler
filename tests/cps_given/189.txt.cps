(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$outfunc2 (void))
 (define g$called (void))
 (define g$outfunc (void))
 (set-then!
  g$outfunc
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (called func k16)
          (set-then!
           called
           0
           (set-then!
            func
            (lambda (k17)
              ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
               (lambda (return k18)
                 ((lambda (k19)
                    ((cps +)
                     called
                     1
                     (lambda (rv20) (set-then! called rv20 (k19 (void))))))
                  k18))
               k17))
            (func
             (lambda (rv21)
               (func (lambda (rv22) ((cps py-print) called k16))))))))
        (void)
        (void)
        k15))
     k14))
  (set-then!
   g$called
   0
   (set-then!
    g$outfunc2
    (lambda (k23)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k24)
         ((lambda (func k25)
            (set-then!
             func
             (lambda (k26)
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (return k27)
                  ((lambda (k28)
                     ((cps +)
                      g$called
                      1
                      (lambda (rv29) (set-then! g$called rv29 (k28 (void))))))
                   k27))
                k26))
             (func (lambda (rv30) (func k25)))))
          (void)
          k24))
       k23))
    (g$outfunc
     (lambda (rv31)
       (g$outfunc2 (lambda (rv32) ((cps py-print) g$called $halt)))))))))
