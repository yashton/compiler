(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$count (void))
 (set-then!
  g$count
  0
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k14)
     ((lambda (loop k15)
        (set-then!
         loop
         (lambda (k16)
           (if #t
             ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
              (lambda (continue k17)
                ((lambda (k18)
                   ((cps +)
                    g$count
                    1
                    (lambda (rv19)
                      (set-then!
                       g$count
                       rv19
                       ((lambda (k20)
                          ((cps >)
                           g$count
                           10
                           (lambda (rv26)
                             (if rv26
                               ((lambda (k27) (break k27)) k20)
                               (k20 (void))))))
                        (lambda (rv21)
                          ((lambda (k22)
                             ((cps equal?)
                              g$count
                              5
                              (lambda (rv24)
                                (if rv24
                                  ((lambda (k25) (continue k25)) k22)
                                  (k22 (void))))))
                           (lambda (rv23) ((cps py-print) g$count k18)))))))))
                 k17))
              (lambda (rv28) (loop k16)))
             (k16 (void))))
         (loop (lambda (rv29) (k15 (void))))))
      (void)
      k14))
   $halt)))
