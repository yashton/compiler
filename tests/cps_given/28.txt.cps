(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$length (void))
 (define g$shape (void))
 (define g$width (void))
 (g$input
  g$menu
  (lambda (rv14)
    (g$int
     rv14
     (lambda (rv15)
       (set-then!
        g$shape
        rv15
        ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
         (lambda (break k16)
           ((lambda (loop k17)
              (set-then!
               loop
               (lambda (k18)
                 ((cps not-equal?)
                  g$shape
                  4
                  (lambda (rv19)
                    (if rv19
                      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                       (lambda (continue k20)
                         ((lambda (k21)
                            ((cps equal?)
                             g$shape
                             1
                             (lambda (rv22)
                               (if rv22
                                 ((lambda (k23)
                                    (g$input
                                     "Length: "
                                     (lambda (rv24)
                                       (g$float
                                        rv24
                                        (lambda (rv25)
                                          (set-then!
                                           g$length
                                           rv25
                                           ((cps expt)
                                            g$length
                                            2
                                            (lambda (rv26)
                                              ((cps py-print)
                                               "Area of square = "
                                               rv26
                                               k23)))))))))
                                  k21)
                                 ((cps equal?)
                                  g$shape
                                  2
                                  (lambda (rv27)
                                    (if rv27
                                      ((lambda (k28)
                                         (g$input
                                          "Length: "
                                          (lambda (rv29)
                                            (g$float
                                             rv29
                                             (lambda (rv30)
                                               (set-then!
                                                g$length
                                                rv30
                                                (g$input
                                                 "Width: "
                                                 (lambda (rv31)
                                                   (g$float
                                                    rv31
                                                    (lambda (rv32)
                                                      (set-then!
                                                       g$width
                                                       rv32
                                                       ((cps *)
                                                        g$length
                                                        g$width
                                                        (lambda (rv33)
                                                          ((cps py-print)
                                                           "Area of rectangle = "
                                                           rv33
                                                           k28))))))))))))))
                                       k21)
                                      ((lambda (k34)
                                         ((cps py-print)
                                          " Not a valid shape. try again"
                                          k34))
                                       k21))))))))
                          k20))
                       (lambda (rv35) (loop k18)))
                      (k18 (void))))))
               (loop (lambda (rv36) (k17 (void))))))
            (void)
            k16))
         $halt)))))))
