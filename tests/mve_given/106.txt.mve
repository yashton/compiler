(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$y (void))
 (define g$x (void))
 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
  (lambda (break k14)
    ((lambda ($seq14 $loop15 k15)
       ((lambda (k16)
          ((cps set?)
           $seq14
           (lambda (rv18)
             (if rv18
               (for-set-k $seq14 $loop15 k16)
               ((cps tuple?)
                $seq14
                (lambda (rv19)
                  (if rv19
                    (for-tuple-k $seq14 $loop15 k16)
                    ((cps py-list?)
                     $seq14
                     (lambda (rv20)
                       (if rv20
                         (for-py-list-k $seq14 $loop15 k16)
                         ((cps dict?)
                          $seq14
                          (lambda (rv21)
                            (if rv21
                              (for-dict-k $seq14 $loop15 k16)
                              (k16 (void)))))))))))))))
        (lambda (rv17) (k15 (void)))))
     (py-list* 1 2 3)
     (lambda (i14 k22)
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (continue k23)
          (set-then!
           g$x
           i14
           ((lambda (k24)
              ((cps py-print)
               g$x
               (lambda (rv25)
                 ((cps +)
                  g$x
                  g$y
                  (lambda (rv26) (set-then! g$y rv26 (k24 (void))))))))
            k23)))
        k22))
     k14))
  $halt))
