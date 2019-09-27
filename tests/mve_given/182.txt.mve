(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fib (void))
 (define g$cache (void))
 (set-then!
  g$cache
  (dict (0 0) (1 1))
  (set-then!
   g$fib
   (lambda (n k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15)
        ((lambda (k16)
           ((lambda (k17)
              ((cps in?)
               n
               g$cache
               (lambda (rv46)
                 (if rv46
                   ((lambda (k47)
                      ((lambda (e15 k48)
                         ((lambda (i14 k49)
                            ((cps py-list?)
                             e15
                             (lambda (rv50)
                               (if rv50
                                 ((cps py-list-ref) e15 i14 k49)
                                 ((cps tuple?)
                                  e15
                                  (lambda (rv51)
                                    (if rv51
                                      ((cps tuple-ref) e15 i14 k49)
                                      ((cps dict?)
                                       e15
                                       (lambda (rv52)
                                         (if rv52
                                           ((cps dict-ref) e15 i14 k49)
                                           (error
                                            "cannot index object"
                                            k49)))))))))))
                          n
                          k48))
                       g$cache
                       (lambda (rv53) (return rv53 k47))))
                    k17)
                   (k17 (void))))))
            (lambda (rv18)
              ((lambda (b17 k19)
                 ((lambda (i16 k20)
                    ((cps tuple?)
                     b17
                     (lambda (rv21)
                       (if rv21
                         ((cps -)
                          n
                          1
                          (lambda (rv22)
                            (g$fib
                             rv22
                             (lambda (rv23)
                               ((cps -)
                                n
                                2
                                (lambda (rv24)
                                  (g$fib
                                   rv24
                                   (lambda (rv25)
                                     ((cps +)
                                      rv23
                                      rv25
                                      (lambda (rv26)
                                        ((cps tuple-set!)
                                         b17
                                         i16
                                         rv26
                                         k20)))))))))))
                         ((cps py-list?)
                          b17
                          (lambda (rv27)
                            (if rv27
                              ((cps -)
                               n
                               1
                               (lambda (rv28)
                                 (g$fib
                                  rv28
                                  (lambda (rv29)
                                    ((cps -)
                                     n
                                     2
                                     (lambda (rv30)
                                       (g$fib
                                        rv30
                                        (lambda (rv31)
                                          ((cps +)
                                           rv29
                                           rv31
                                           (lambda (rv32)
                                             ((cps py-list-set!)
                                              b17
                                              i16
                                              rv32
                                              k20)))))))))))
                              ((cps dict?)
                               b17
                               (lambda (rv33)
                                 (if rv33
                                   ((cps -)
                                    n
                                    1
                                    (lambda (rv34)
                                      (g$fib
                                       rv34
                                       (lambda (rv35)
                                         ((cps -)
                                          n
                                          2
                                          (lambda (rv36)
                                            (g$fib
                                             rv36
                                             (lambda (rv37)
                                               ((cps +)
                                                rv35
                                                rv37
                                                (lambda (rv38)
                                                  ((cps dict-set!)
                                                   b17
                                                   i16
                                                   rv38
                                                   k20)))))))))))
                                   (k20 (void))))))))))))
                  n
                  k19))
               g$cache
               (lambda (rv39)
                 ((lambda (e19 k40)
                    ((lambda (i18 k41)
                       ((cps py-list?)
                        e19
                        (lambda (rv42)
                          (if rv42
                            ((cps py-list-ref) e19 i18 k41)
                            ((cps tuple?)
                             e19
                             (lambda (rv43)
                               (if rv43
                                 ((cps tuple-ref) e19 i18 k41)
                                 ((cps dict?)
                                  e19
                                  (lambda (rv44)
                                    (if rv44
                                      ((cps dict-ref) e19 i18 k41)
                                      (error
                                       "cannot index object"
                                       k41)))))))))))
                     n
                     k40))
                  g$cache
                  (lambda (rv45) (return rv45 k16))))))))
         k15))
      k14))
   (g$fib 25 (lambda (rv54) ((cps py-print) rv54 $halt))))))
