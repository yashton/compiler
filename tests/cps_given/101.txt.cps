(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$abs (void))
 (set-then!
  g$abs
  (lambda (x k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps <)
           x
           0
           (lambda (rv17)
             (if rv17
               ((lambda (k18) ((cps -) x (lambda (rv19) (return rv19 k18))))
                k16)
               ((lambda (k20) (return x k20)) k16)))))
        k15))
     k14))
  ((cps -)
   1
   (lambda (rv21)
     ((cps -)
      2
      (lambda (rv22)
        ((cps *)
         rv21
         rv22
         (lambda (rv23)
           ((cps -)
            3
            (lambda (rv24)
              ((cps *)
               rv23
               rv24
               (lambda (rv25)
                 ((cps -)
                  4
                  (lambda (rv26)
                    ((cps *)
                     rv25
                     rv26
                     (lambda (rv27)
                       ((cps -)
                        5
                        (lambda (rv28)
                          ((cps *)
                           rv27
                           rv28
                           (lambda (rv29)
                             ((cps -)
                              6
                              (lambda (rv30)
                                ((cps *)
                                 rv29
                                 rv30
                                 (lambda (rv31)
                                   (g$abs
                                    rv31
                                    (lambda (rv32)
                                      ((cps py-print)
                                       rv32
                                       $halt)))))))))))))))))))))))))))
