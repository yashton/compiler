(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$g (void))
 (define g$a (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps py-print) "called f" (lambda (rv17) (return 1 k16))))
        k15))
     k14))
  (set-then!
   g$g
   (lambda (k18)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k19)
        ((lambda (k20)
           ((cps py-print) "called g" (lambda (rv21) (return 0 k20))))
         k19))
      k18))
   (set-then!
    g$a
    (py-list* (py-list* 10 20) (py-list* 30 40) (py-list* 50 60))
    ((lambda (e15 k38)
       (g$f
        (lambda (rv43)
          ((lambda (i14 k39)
             ((cps py-list?)
              e15
              (lambda (rv40)
                (if rv40
                  ((cps py-list-ref) e15 i14 k39)
                  ((cps tuple?)
                   e15
                   (lambda (rv41)
                     (if rv41
                       ((cps tuple-ref) e15 i14 k39)
                       ((cps dict?)
                        e15
                        (lambda (rv42)
                          (if rv42
                            ((cps dict-ref) e15 i14 k39)
                            (error "cannot index object" k39)))))))))))
           rv43
           k38))))
     g$a
     (lambda (rv44)
       ((lambda (b17 k22)
          (g$g
           (lambda (rv37)
             ((lambda (i16 k23)
                ((lambda (e20 k31)
                   ((lambda (i19 k32)
                      ((cps py-list?)
                       e20
                       (lambda (rv33)
                         (if rv33
                           ((cps py-list-ref) e20 i19 k32)
                           ((cps tuple?)
                            e20
                            (lambda (rv34)
                              (if rv34
                                ((cps tuple-ref) e20 i19 k32)
                                ((cps dict?)
                                 e20
                                 (lambda (rv35)
                                   (if rv35
                                     ((cps dict-ref) e20 i19 k32)
                                     (error
                                      "cannot index object"
                                      k32)))))))))))
                    i16
                    k31))
                 b17
                 (lambda (rv36)
                   ((lambda (v18 k24)
                      ((cps tuple?)
                       b17
                       (lambda (rv25)
                         (if rv25
                           ((cps +)
                            v18
                            30
                            (lambda (rv26)
                              ((cps tuple-set!) b17 i16 rv26 k24)))
                           ((cps py-list?)
                            b17
                            (lambda (rv27)
                              (if rv27
                                ((cps +)
                                 v18
                                 30
                                 (lambda (rv28)
                                   ((cps py-list-set!) b17 i16 rv28 k24)))
                                ((cps dict?)
                                 b17
                                 (lambda (rv29)
                                   (if rv29
                                     ((cps +)
                                      v18
                                      30
                                      (lambda (rv30)
                                        ((cps dict-set!) b17 i16 rv30 k24)))
                                     (k24 (void))))))))))))
                    rv36
                    k23))))
              rv37
              k22))))
        rv44
        (lambda (rv45)
          ((lambda (e22 k46)
             (g$f
              (lambda (rv51)
                ((lambda (i21 k47)
                   ((cps py-list?)
                    e22
                    (lambda (rv48)
                      (if rv48
                        ((cps py-list-ref) e22 i21 k47)
                        ((cps tuple?)
                         e22
                         (lambda (rv49)
                           (if rv49
                             ((cps tuple-ref) e22 i21 k47)
                             ((cps dict?)
                              e22
                              (lambda (rv50)
                                (if rv50
                                  ((cps dict-ref) e22 i21 k47)
                                  (error "cannot index object" k47)))))))))))
                 rv51
                 k46))))
           g$a
           (lambda (rv52) ((cps py-print) rv52 $halt)))))))))))
