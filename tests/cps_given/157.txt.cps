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
      1
      (lambda (rv22)
        ((cps *)
         rv21
         rv22
         (lambda (rv23)
           ((cps -)
            1
            (lambda (rv24)
              ((cps *)
               rv23
               rv24
               (lambda (rv25)
                 (g$abs
                  rv25
                  (lambda (rv26) ((cps py-print) rv26 $halt)))))))))))))))
