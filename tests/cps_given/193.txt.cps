(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$main (void))
 (define g$moveit (void))
 (define g$dohanoi (void))
 (set-then!
  g$moveit
  (lambda (frm to k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((cps py-print)
           frm
           (lambda (rv17)
             ((cps py-print) to (lambda (rv18) ((cps py-print) "---" k16))))))
        k15))
     k14))
  (set-then!
   g$dohanoi
   (lambda (n to frm using k19)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k20)
        ((lambda (k21)
           ((lambda (k22)
              ((cps equal?)
               n
               0
               (lambda (rv28) (if rv28 (return (py-list*) k22) (k22 (void))))))
            (lambda (rv23)
              ((cps -)
               n
               1
               (lambda (rv24)
                 (g$dohanoi
                  rv24
                  using
                  frm
                  to
                  (lambda (rv25)
                    (g$moveit
                     frm
                     to
                     (lambda (rv26)
                       ((cps -)
                        n
                        1
                        (lambda (rv27)
                          (g$dohanoi rv27 to using frm k21))))))))))))
         k20))
      k19))
   (set-then!
    g$main
    (lambda (k29)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k30)
         ((lambda (k31)
            ((lambda ($old-handler k32)
               ((lambda ($old-return k33)
                  ((lambda ($old-continue k34)
                     ((lambda ($old-break k35)
                        ((lambda (return k36)
                           ((lambda (continue k37)
                              ((lambda (break k38)
                                 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                  (lambda ($ec15 k39)
                                    (set-then!
                                     $current-handler
                                     (lambda ($ex14 k40)
                                       (set-then!
                                        $current-handler
                                        $old-handler
                                        ((lambda (ex k41)
                                           ((lambda (k42) (k42 (void))) k41))
                                         $ex14
                                         (lambda (rv43) ($ec15 rv43 k40)))))
                                     ((lambda (k45) (g$dohanoi 2 3 1 2 k45))
                                      (lambda (rv46)
                                        ((lambda (rv k44)
                                           (set-then!
                                            $current-handler
                                            $old-handler
                                            (k44 rv)))
                                         rv46
                                         k39)))))
                                  k38))
                               (lambda (k47)
                                 (set-then!
                                  $current-handler
                                  $old-handler
                                  ($old-break k47)))
                               k37))
                            (lambda (k48)
                              (set-then!
                               $current-handler
                               $old-handler
                               ($old-continue k48)))
                            k36))
                         (lambda (rv k49)
                           (set-then!
                            $current-handler
                            $old-handler
                            (return rv k49)))
                         k35))
                      break
                      k34))
                   continue
                   k33))
                return
                k32))
             $current-handler
             k31))
          k30))
       k29))
    (g$main $halt)))))
