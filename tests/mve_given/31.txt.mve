(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 ((lambda (k17)
    ((cps <)
     g$a
     g$b
     (lambda (rv20)
       (if rv20
         ((cps <=)
          g$b
          g$c
          (lambda (rv21) (if rv21 ((cps >) g$c g$d k17) (k17 #f))))
         (k17 #f)))))
  (lambda (rv18)
    ((lambda (t14 k14)
       (if t14
         (k14 t14)
         ((cps >=)
          g$z
          g$f
          (lambda (rv16)
            ((lambda (t15 k15) (if t15 (k15 t15) (k15 g$a))) rv16 k14)))))
     rv18
     (lambda (rv19) (set-then! g$b rv19 ($halt (void))))))))
