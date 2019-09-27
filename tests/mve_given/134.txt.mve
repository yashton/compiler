(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda ($old-handler k14)
    ((lambda ($old-return k15)
       ((lambda ($old-continue k16)
          ((lambda ($old-break k17)
             ((lambda (return k18)
                ((lambda (continue k19)
                   ((lambda (break k20)
                      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                       (lambda ($ec15 k21)
                         (set-then!
                          $current-handler
                          (lambda ($ex14 k22)
                            (set-then!
                             $current-handler
                             $old-handler
                             ((lambda (ex k23)
                                ((lambda (k24)
                                   ((cps py-print) "excepted!" k24))
                                 k23))
                              $ex14
                              (lambda (rv25) ($ec15 rv25 k22)))))
                          ((lambda (k27) ($current-handler "boo" k27))
                           (lambda (rv28)
                             ((lambda (rv k26)
                                (set-then!
                                 $current-handler
                                 $old-handler
                                 (k26 rv)))
                              rv28
                              k21)))))
                       k20))
                    (lambda (k29)
                      (set-then!
                       $current-handler
                       $old-handler
                       ($old-break k29)))
                    k19))
                 (lambda (k30)
                   (set-then!
                    $current-handler
                    $old-handler
                    ($old-continue k30)))
                 k18))
              (lambda (rv k31)
                (set-then! $current-handler $old-handler (return rv k31)))
              k17))
           break
           k16))
        continue
        k15))
     return
     k14))
  $current-handler
  $halt))
