(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 0 1 2 3)
  ((lambda (b15 k14)
     ((lambda (i14 k15)
        ((cps tuple?)
         b15
         (lambda (rv16)
           (if rv16
             ((cps tuple-set!) b15 i14 30 k15)
             ((cps py-list?)
              b15
              (lambda (rv17)
                (if rv17
                  ((cps py-list-set!) b15 i14 30 k15)
                  ((cps dict?)
                   b15
                   (lambda (rv18)
                     (if rv18
                       ((cps dict-set!) b15 i14 30 k15)
                       (k15 (void))))))))))))
      1
      k14))
   g$a
   (lambda (rv19) ((cps py-print) g$a $halt)))))
