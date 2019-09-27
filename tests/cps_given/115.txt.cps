(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 1 2 3)
  ((lambda (e15 k14)
     ((lambda (i14 k15)
        ((cps py-list?)
         e15
         (lambda (rv16)
           (if rv16
             ((cps py-list-ref) e15 i14 k15)
             ((cps tuple?)
              e15
              (lambda (rv17)
                (if rv17
                  ((cps tuple-ref) e15 i14 k15)
                  ((cps dict?)
                   e15
                   (lambda (rv18)
                     (if rv18
                       ((cps dict-ref) e15 i14 k15)
                       (error "cannot index object" k15)))))))))))
      1
      k14))
   g$a
   (lambda (rv19) ((cps py-print) rv19 $halt)))))
