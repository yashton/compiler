(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$doomed (void))
 (set-then!
  g$doomed
  (lambda (k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (k16)
          ((lambda ($old-handler k17)
             ((lambda ($old-return k18)
                ((lambda ($old-continue k19)
                   ((lambda ($old-break k20)
                      ((lambda (return k21)
                         ((lambda (continue k22)
                            ((lambda (break k23)
                               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                (lambda ($ec15 k24)
                                  (set-then!
                                   $current-handler
                                   (lambda ($ex14 k25)
                                     (set-then!
                                      $current-handler
                                      $old-handler
                                      ((lambda (ex k26)
                                         ((lambda (k27)
                                            ((cps py-print)
                                             "caught an index error!"
                                             k27))
                                          k26))
                                       $ex14
                                       (lambda (rv28) ($ec15 rv28 k25)))))
                                   ((lambda (k30)
                                      ((cps py-print) "try...\n" k30))
                                    (lambda (rv31)
                                      ((lambda (rv k29)
                                         (set-then!
                                          $current-handler
                                          $old-handler
                                          (k29 rv)))
                                       rv31
                                       k24)))))
                                k23))
                             (lambda (k32)
                               (set-then!
                                $current-handler
                                $old-handler
                                ($old-break k32)))
                             k22))
                          (lambda (k33)
                            (set-then!
                             $current-handler
                             $old-handler
                             ($old-continue k33)))
                          k21))
                       (lambda (rv k34)
                         (set-then!
                          $current-handler
                          $old-handler
                          (return rv k34)))
                       k20))
                    break
                    k19))
                 continue
                 k18))
              return
              k17))
           $current-handler
           k16))
        k15))
     k14))
  (g$doomed $halt)))
