(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$v (void))
 (define g$z (void))
 (define g$x (void))
 (define g$y (void))
 (set-then!
  g$v
  (tuple "a" "b" "e")
  ((lambda (t14 k14)
     ((lambda (e16 k15)
        ((lambda (i15 k16)
           ((cps py-list?)
            e16
            (lambda (rv17)
              (if rv17
                ((cps py-list-ref) e16 i15 k16)
                ((cps tuple?)
                 e16
                 (lambda (rv18)
                   (if rv18
                     ((cps tuple-ref) e16 i15 k16)
                     ((cps dict?)
                      e16
                      (lambda (rv19)
                        (if rv19
                          ((cps dict-ref) e16 i15 k16)
                          (error "cannot index object" k16)))))))))))
         0
         k15))
      t14
      (lambda (rv20)
        (set-then!
         g$x
         rv20
         ((lambda (e18 k21)
            ((lambda (i17 k22)
               ((cps py-list?)
                e18
                (lambda (rv23)
                  (if rv23
                    ((cps py-list-ref) e18 i17 k22)
                    ((cps tuple?)
                     e18
                     (lambda (rv24)
                       (if rv24
                         ((cps tuple-ref) e18 i17 k22)
                         ((cps dict?)
                          e18
                          (lambda (rv25)
                            (if rv25
                              ((cps dict-ref) e18 i17 k22)
                              (error "cannot index object" k22)))))))))))
             1
             k21))
          t14
          (lambda (rv26)
            (set-then!
             g$y
             rv26
             ((lambda (e20 k27)
                ((lambda (i19 k28)
                   ((cps py-list?)
                    e20
                    (lambda (rv29)
                      (if rv29
                        ((cps py-list-ref) e20 i19 k28)
                        ((cps tuple?)
                         e20
                         (lambda (rv30)
                           (if rv30
                             ((cps tuple-ref) e20 i19 k28)
                             ((cps dict?)
                              e20
                              (lambda (rv31)
                                (if rv31
                                  ((cps dict-ref) e20 i19 k28)
                                  (error "cannot index object" k28)))))))))))
                 2
                 k27))
              t14
              (lambda (rv32) (set-then! g$z rv32 (k14 (void))))))))))))
   g$v
   (lambda (rv33)
     ((cps py-print)
      g$x
      (lambda (rv34)
        ((cps py-print) g$y (lambda (rv35) ((cps py-print) g$z $halt)))))))))
