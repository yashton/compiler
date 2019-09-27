(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  (py-list* 1 2 3)
  (set-then!
   g$b
   (py-list* 1 2 3)
   ((lambda (b15 k14)
      ((lambda (i14 k15)
         ((cps tuple?)
          b15
          (lambda (rv16)
            (if rv16
              ((lambda (e17 k17)
                 ((lambda (i16 k18)
                    ((cps py-list?)
                     e17
                     (lambda (rv19)
                       (if rv19
                         ((cps py-list-ref) e17 i16 k18)
                         ((cps tuple?)
                          e17
                          (lambda (rv20)
                            (if rv20
                              ((cps tuple-ref) e17 i16 k18)
                              ((cps dict?)
                               e17
                               (lambda (rv21)
                                 (if rv21
                                   ((cps dict-ref) e17 i16 k18)
                                   (error "cannot index object" k18)))))))))))
                  4
                  k17))
               g$a
               (lambda (rv22) ((cps tuple-set!) b15 i14 rv22 k15)))
              ((cps py-list?)
               b15
               (lambda (rv23)
                 (if rv23
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
                                        (error
                                         "cannot index object"
                                         k25)))))))))))
                       4
                       k24))
                    g$a
                    (lambda (rv29) ((cps py-list-set!) b15 i14 rv29 k15)))
                   ((cps dict?)
                    b15
                    (lambda (rv30)
                      (if rv30
                        ((lambda (e21 k31)
                           ((lambda (i20 k32)
                              ((cps py-list?)
                               e21
                               (lambda (rv33)
                                 (if rv33
                                   ((cps py-list-ref) e21 i20 k32)
                                   ((cps tuple?)
                                    e21
                                    (lambda (rv34)
                                      (if rv34
                                        ((cps tuple-ref) e21 i20 k32)
                                        ((cps dict?)
                                         e21
                                         (lambda (rv35)
                                           (if rv35
                                             ((cps dict-ref) e21 i20 k32)
                                             (error
                                              "cannot index object"
                                              k32)))))))))))
                            4
                            k31))
                         g$a
                         (lambda (rv36) ((cps dict-set!) b15 i14 rv36 k15)))
                        (k15 (void))))))))))))
       1
       k14))
    g$a
    $halt))))
