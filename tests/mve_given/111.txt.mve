(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (e15 k34)
    ((lambda (i14 k35)
       ((cps py-list?)
        e15
        (lambda (rv36)
          (if rv36
            ((cps py-list-ref) e15 i14 k35)
            ((cps tuple?)
             e15
             (lambda (rv37)
               (if rv37
                 ((cps tuple-ref) e15 i14 k35)
                 ((cps dict?)
                  e15
                  (lambda (rv38)
                    (if rv38
                      ((cps dict-ref) e15 i14 k35)
                      (error "cannot index object" k35)))))))))))
     1
     k34))
  g$a
  (lambda (rv39)
    ((lambda (e17 k29)
       ((lambda (i16 k30)
          ((cps py-list?)
           e17
           (lambda (rv31)
             (if rv31
               ((cps py-list-ref) e17 i16 k30)
               ((cps tuple?)
                e17
                (lambda (rv32)
                  (if rv32
                    ((cps tuple-ref) e17 i16 k30)
                    ((cps dict?)
                     e17
                     (lambda (rv33)
                       (if rv33
                         ((cps dict-ref) e17 i16 k30)
                         (error "cannot index object" k30)))))))))))
        2
        k29))
     rv39
     (lambda (rv40)
       ((lambda (e19 k24)
          ((lambda (i18 k25)
             ((cps py-list?)
              e19
              (lambda (rv26)
                (if rv26
                  ((cps py-list-ref) e19 i18 k25)
                  ((cps tuple?)
                   e19
                   (lambda (rv27)
                     (if rv27
                       ((cps tuple-ref) e19 i18 k25)
                       ((cps dict?)
                        e19
                        (lambda (rv28)
                          (if rv28
                            ((cps dict-ref) e19 i18 k25)
                            (error "cannot index object" k25)))))))))))
           3
           k24))
        rv40
        (lambda (rv41)
          ((lambda (e21 k19)
             ((lambda (i20 k20)
                ((cps py-list?)
                 e21
                 (lambda (rv21)
                   (if rv21
                     ((cps py-list-ref) e21 i20 k20)
                     ((cps tuple?)
                      e21
                      (lambda (rv22)
                        (if rv22
                          ((cps tuple-ref) e21 i20 k20)
                          ((cps dict?)
                           e21
                           (lambda (rv23)
                             (if rv23
                               ((cps dict-ref) e21 i20 k20)
                               (error "cannot index object" k20)))))))))))
              4
              k19))
           rv41
           (lambda (rv42)
             ((lambda (e23 k14)
                ((lambda (i22 k15)
                   ((cps py-list?)
                    e23
                    (lambda (rv16)
                      (if rv16
                        ((cps py-list-ref) e23 i22 k15)
                        ((cps tuple?)
                         e23
                         (lambda (rv17)
                           (if rv17
                             ((cps tuple-ref) e23 i22 k15)
                             ((cps dict?)
                              e23
                              (lambda (rv18)
                                (if rv18
                                  ((cps dict-ref) e23 i22 k15)
                                  (error "cannot index object" k15)))))))))))
                 5
                 k14))
              rv42
              $halt))))))))))
