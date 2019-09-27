(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  (dict (1 2) (3 4))
  (set-then!
   g$b
   (py-list* 0 1 2)
   ((lambda (b15 k14)
      ((lambda (i14 k15)
         ((cps tuple?)
          b15
          (lambda (rv16)
            (if rv16
              (error "Cannot delete from tuples!" k15)
              ((cps py-list?)
               b15
               (lambda (rv17)
                 (if rv17
                   ((cps py-list-remove!) b15 i14 k15)
                   ((cps dict?)
                    b15
                    (lambda (rv18)
                      (if rv18
                        ((cps dict-remove!) b15 i14 k15)
                        (k15 (void))))))))))))
       1
       k14))
    g$a
    (lambda (rv19)
      ((lambda (b17 k20)
         ((lambda (i16 k21)
            ((cps tuple?)
             b17
             (lambda (rv22)
               (if rv22
                 (error "Cannot delete from tuples!" k21)
                 ((cps py-list?)
                  b17
                  (lambda (rv23)
                    (if rv23
                      ((cps py-list-remove!) b17 i16 k21)
                      ((cps dict?)
                       b17
                       (lambda (rv24)
                         (if rv24
                           ((cps dict-remove!) b17 i16 k21)
                           (k21 (void))))))))))))
          2
          k20))
       g$b
       (lambda (rv25)
         ((cps py-print) g$a (lambda (rv26) ((cps py-print) g$b $halt))))))))))
