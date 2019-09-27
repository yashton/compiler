(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a_bigger (void))
 (set-then!
  g$a_bigger
  (lambda (a b k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((lambda (k17)
             ((cps >)
              a
              b
              (lambda (rv21)
                (if rv21
                  ((cps -) a b (lambda (rv22) ((cps >=) rv22 2 k17)))
                  (k17 #f)))))
           (lambda (rv18)
             (if rv18
               ((lambda (k19) (return #t k19)) k16)
               ((lambda (k20) (return #f k20)) k16)))))
        k15))
     k14))
  ($halt (void))))
