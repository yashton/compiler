(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$sum (void))
 (define g$i (void))
 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
  (lambda (break k14)
    (g$range
     10
     (lambda (rv22)
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
        rv22
        (lambda (i14 k23)
          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
           (lambda (continue k24)
             (set-then!
              g$i
              i14
              ((lambda (k25)
                 ((cps +)
                  g$sum
                  1
                  (lambda (rv26) (set-then! g$sum rv26 (k25 (void))))))
               k24)))
           k23))
        k14))))
  $halt))
