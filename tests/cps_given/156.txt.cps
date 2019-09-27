(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
  (lambda (break k14)
    ((lambda (loop k15)
       (set-then!
        loop
        (lambda (k16)
          (if #f
            ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
             (lambda (continue k17)
               ((lambda (k18) ((cps py-print) "here" k18)) k17))
             (lambda (rv19) (loop k16)))
            (k16 (void))))
        (loop (lambda (rv20) (k15 (void))))))
     (void)
     k14))
  $halt))
