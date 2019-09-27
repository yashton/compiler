(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$g (void))
 (set-then!
  g$g
  200
  (set-then!
   g$f
   (lambda (k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15)
        ((lambda (h x k16)
           ((lambda (k17)
              (if #f ((lambda (k35) (k35 (void))) k17) (k17 (void))))
            (lambda (rv18)
              ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
               (lambda (break k19)
                 ((lambda ($seq14 $loop15 k20)
                    ((lambda (k21)
                       ((cps set?)
                        $seq14
                        (lambda (rv23)
                          (if rv23
                            (for-set-k $seq14 $loop15 k21)
                            ((cps tuple?)
                             $seq14
                             (lambda (rv24)
                               (if rv24
                                 (for-tuple-k $seq14 $loop15 k21)
                                 ((cps py-list?)
                                  $seq14
                                  (lambda (rv25)
                                    (if rv25
                                      (for-py-list-k $seq14 $loop15 k21)
                                      ((cps dict?)
                                       $seq14
                                       (lambda (rv26)
                                         (if rv26
                                           (for-dict-k $seq14 $loop15 k21)
                                           (k21 (void)))))))))))))))
                     (lambda (rv22) (k20 (void)))))
                  (py-list* 1 2 3)
                  (lambda (i14 k27)
                    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                     (lambda (continue k28) (set-then! g$g i14 (k28 (void))))
                     k27))
                  k19))
               (lambda (rv29)
                 (set-then!
                  x
                  314
                  (set-then!
                   h
                   (lambda (k30)
                     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                      (lambda (return k31)
                        ((lambda (g k32)
                           (set-then! g x ((cps py-print) g k32)))
                         (void)
                         k31))
                      k30))
                   (h
                    (lambda (rv33)
                      ((cps py-print)
                       g$g
                       (lambda (rv34) (return g$g k16))))))))))))
         (void)
         (void)
         k15))
      k14))
   (g$f
    (lambda (rv36)
      ((cps py-print) rv36 (lambda (rv37) ((cps py-print) g$g $halt))))))))
