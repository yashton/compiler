(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$result (void))
 (define g$unsort (void))
 (define g$BubbleSort (void))
 (set-then!
  g$BubbleSort
  (lambda (lst k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (passesLeft i k16)
          (g$list
           lst
           (lambda (rv17)
             (set-then!
              lst
              rv17
              ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
               (lambda (break k18)
                 (g$len
                  lst
                  (lambda (rv26)
                    ((cps -)
                     rv26
                     1
                     (lambda (rv27)
                       ((cps -)
                        1
                        (lambda (rv28)
                          (g$range
                           rv27
                           0
                           rv28
                           (lambda (rv29)
                             ((lambda ($seq14 $loop15 k19)
                                ((lambda (k20)
                                   ((cps set?)
                                    $seq14
                                    (lambda (rv22)
                                      (if rv22
                                        (for-set-k $seq14 $loop15 k20)
                                        ((cps tuple?)
                                         $seq14
                                         (lambda (rv23)
                                           (if rv23
                                             (for-tuple-k $seq14 $loop15 k20)
                                             ((cps py-list?)
                                              $seq14
                                              (lambda (rv24)
                                                (if rv24
                                                  (for-py-list-k
                                                   $seq14
                                                   $loop15
                                                   k20)
                                                  ((cps dict?)
                                                   $seq14
                                                   (lambda (rv25)
                                                     (if rv25
                                                       (for-dict-k
                                                        $seq14
                                                        $loop15
                                                        k20)
                                                       (k20
                                                        (void)))))))))))))))
                                 (lambda (rv21) (k19 (void)))))
                              rv29
                              (lambda (i14 k30)
                                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                 (lambda (continue k31)
                                   (set-then!
                                    passesLeft
                                    i14
                                    ((lambda (k32)
                                       ((lambda (f cc)
                                          (f (lambda (x k) (cc x)) cc))
                                        (lambda (break k33)
                                          (g$range
                                           passesLeft
                                           (lambda (rv41)
                                             ((lambda ($seq16 $loop17 k34)
                                                ((lambda (k35)
                                                   ((cps set?)
                                                    $seq16
                                                    (lambda (rv37)
                                                      (if rv37
                                                        (for-set-k
                                                         $seq16
                                                         $loop17
                                                         k35)
                                                        ((cps tuple?)
                                                         $seq16
                                                         (lambda (rv38)
                                                           (if rv38
                                                             (for-tuple-k
                                                              $seq16
                                                              $loop17
                                                              k35)
                                                             ((cps py-list?)
                                                              $seq16
                                                              (lambda (rv39)
                                                                (if rv39
                                                                  (for-py-list-k
                                                                   $seq16
                                                                   $loop17
                                                                   k35)
                                                                  ((cps dict?)
                                                                   $seq16
                                                                   (lambda (rv40)
                                                                     (if rv40
                                                                       (for-dict-k
                                                                        $seq16
                                                                        $loop17
                                                                        k35)
                                                                       (k35
                                                                        (void)))))))))))))))
                                                 (lambda (rv36) (k34 (void)))))
                                              rv41
                                              (lambda (i15 k42)
                                                ((lambda (f cc)
                                                   (f
                                                    (lambda (x k) (cc x))
                                                    cc))
                                                 (lambda (continue k43)
                                                   (set-then!
                                                    i
                                                    i15
                                                    ((lambda (k44)
                                                       ((lambda (e17 k45)
                                                          ((lambda (i16 k46)
                                                             ((cps py-list?)
                                                              e17
                                                              (lambda (rv47)
                                                                (if rv47
                                                                  ((cps
                                                                    py-list-ref)
                                                                   e17
                                                                   i16
                                                                   k46)
                                                                  ((cps tuple?)
                                                                   e17
                                                                   (lambda (rv48)
                                                                     (if rv48
                                                                       ((cps
                                                                         tuple-ref)
                                                                        e17
                                                                        i16
                                                                        k46)
                                                                       ((cps
                                                                         dict?)
                                                                        e17
                                                                        (lambda (rv49)
                                                                          (if rv49
                                                                            ((cps
                                                                              dict-ref)
                                                                             e17
                                                                             i16
                                                                             k46)
                                                                            (error
                                                                             "cannot index object"
                                                                             k46)))))))))))
                                                           i
                                                           k45))
                                                        lst
                                                        (lambda (rv50)
                                                          ((lambda (e19 k51)
                                                             ((cps +)
                                                              i
                                                              1
                                                              (lambda (rv56)
                                                                ((lambda (i18
                                                                          k52)
                                                                   ((cps
                                                                     py-list?)
                                                                    e19
                                                                    (lambda (rv53)
                                                                      (if rv53
                                                                        ((cps
                                                                          py-list-ref)
                                                                         e19
                                                                         i18
                                                                         k52)
                                                                        ((cps
                                                                          tuple?)
                                                                         e19
                                                                         (lambda (rv54)
                                                                           (if rv54
                                                                             ((cps
                                                                               tuple-ref)
                                                                              e19
                                                                              i18
                                                                              k52)
                                                                             ((cps
                                                                               dict?)
                                                                              e19
                                                                              (lambda (rv55)
                                                                                (if rv55
                                                                                  ((cps
                                                                                    dict-ref)
                                                                                   e19
                                                                                   i18
                                                                                   k52)
                                                                                  (error
                                                                                   "cannot index object"
                                                                                   k52)))))))))))
                                                                 rv56
                                                                 k51))))
                                                           lst
                                                           (lambda (rv57)
                                                             ((cps <)
                                                              rv50
                                                              rv57
                                                              (lambda (rv58)
                                                                (if rv58
                                                                  ((lambda (k59)
                                                                     ((lambda (e22
                                                                               k109)
                                                                        ((cps
                                                                          +)
                                                                         i
                                                                         1
                                                                         (lambda (rv114)
                                                                           ((lambda (i21
                                                                                     k110)
                                                                              ((cps
                                                                                py-list?)
                                                                               e22
                                                                               (lambda (rv111)
                                                                                 (if rv111
                                                                                   ((cps
                                                                                     py-list-ref)
                                                                                    e22
                                                                                    i21
                                                                                    k110)
                                                                                   ((cps
                                                                                     tuple?)
                                                                                    e22
                                                                                    (lambda (rv112)
                                                                                      (if rv112
                                                                                        ((cps
                                                                                          tuple-ref)
                                                                                         e22
                                                                                         i21
                                                                                         k110)
                                                                                        ((cps
                                                                                          dict?)
                                                                                         e22
                                                                                         (lambda (rv113)
                                                                                           (if rv113
                                                                                             ((cps
                                                                                               dict-ref)
                                                                                              e22
                                                                                              i21
                                                                                              k110)
                                                                                             (error
                                                                                              "cannot index object"
                                                                                              k110)))))))))))
                                                                            rv114
                                                                            k109))))
                                                                      lst
                                                                      (lambda (rv115)
                                                                        ((lambda (e24
                                                                                  k116)
                                                                           ((lambda (i23
                                                                                     k117)
                                                                              ((cps
                                                                                py-list?)
                                                                               e24
                                                                               (lambda (rv118)
                                                                                 (if rv118
                                                                                   ((cps
                                                                                     py-list-ref)
                                                                                    e24
                                                                                    i23
                                                                                    k117)
                                                                                   ((cps
                                                                                     tuple?)
                                                                                    e24
                                                                                    (lambda (rv119)
                                                                                      (if rv119
                                                                                        ((cps
                                                                                          tuple-ref)
                                                                                         e24
                                                                                         i23
                                                                                         k117)
                                                                                        ((cps
                                                                                          dict?)
                                                                                         e24
                                                                                         (lambda (rv120)
                                                                                           (if rv120
                                                                                             ((cps
                                                                                               dict-ref)
                                                                                              e24
                                                                                              i23
                                                                                              k117)
                                                                                             (error
                                                                                              "cannot index object"
                                                                                              k117)))))))))))
                                                                            i
                                                                            k116))
                                                                         lst
                                                                         (lambda (rv121)
                                                                           ((lambda (t20
                                                                                     k60)
                                                                              ((lambda (b26
                                                                                        k61)
                                                                                 ((lambda (i25
                                                                                           k62)
                                                                                    ((cps
                                                                                      tuple?)
                                                                                     b26
                                                                                     (lambda (rv63)
                                                                                       (if rv63
                                                                                         ((lambda (e28
                                                                                                   k64)
                                                                                            ((lambda (i27
                                                                                                      k65)
                                                                                               ((cps
                                                                                                 py-list?)
                                                                                                e28
                                                                                                (lambda (rv66)
                                                                                                  (if rv66
                                                                                                    ((cps
                                                                                                      py-list-ref)
                                                                                                     e28
                                                                                                     i27
                                                                                                     k65)
                                                                                                    ((cps
                                                                                                      tuple?)
                                                                                                     e28
                                                                                                     (lambda (rv67)
                                                                                                       (if rv67
                                                                                                         ((cps
                                                                                                           tuple-ref)
                                                                                                          e28
                                                                                                          i27
                                                                                                          k65)
                                                                                                         ((cps
                                                                                                           dict?)
                                                                                                          e28
                                                                                                          (lambda (rv68)
                                                                                                            (if rv68
                                                                                                              ((cps
                                                                                                                dict-ref)
                                                                                                               e28
                                                                                                               i27
                                                                                                               k65)
                                                                                                              (error
                                                                                                               "cannot index object"
                                                                                                               k65)))))))))))
                                                                                             0
                                                                                             k64))
                                                                                          t20
                                                                                          (lambda (rv69)
                                                                                            ((cps
                                                                                              tuple-set!)
                                                                                             b26
                                                                                             i25
                                                                                             rv69
                                                                                             k62)))
                                                                                         ((cps
                                                                                           py-list?)
                                                                                          b26
                                                                                          (lambda (rv70)
                                                                                            (if rv70
                                                                                              ((lambda (e30
                                                                                                        k71)
                                                                                                 ((lambda (i29
                                                                                                           k72)
                                                                                                    ((cps
                                                                                                      py-list?)
                                                                                                     e30
                                                                                                     (lambda (rv73)
                                                                                                       (if rv73
                                                                                                         ((cps
                                                                                                           py-list-ref)
                                                                                                          e30
                                                                                                          i29
                                                                                                          k72)
                                                                                                         ((cps
                                                                                                           tuple?)
                                                                                                          e30
                                                                                                          (lambda (rv74)
                                                                                                            (if rv74
                                                                                                              ((cps
                                                                                                                tuple-ref)
                                                                                                               e30
                                                                                                               i29
                                                                                                               k72)
                                                                                                              ((cps
                                                                                                                dict?)
                                                                                                               e30
                                                                                                               (lambda (rv75)
                                                                                                                 (if rv75
                                                                                                                   ((cps
                                                                                                                     dict-ref)
                                                                                                                    e30
                                                                                                                    i29
                                                                                                                    k72)
                                                                                                                   (error
                                                                                                                    "cannot index object"
                                                                                                                    k72)))))))))))
                                                                                                  0
                                                                                                  k71))
                                                                                               t20
                                                                                               (lambda (rv76)
                                                                                                 ((cps
                                                                                                   py-list-set!)
                                                                                                  b26
                                                                                                  i25
                                                                                                  rv76
                                                                                                  k62)))
                                                                                              ((cps
                                                                                                dict?)
                                                                                               b26
                                                                                               (lambda (rv77)
                                                                                                 (if rv77
                                                                                                   ((lambda (e32
                                                                                                             k78)
                                                                                                      ((lambda (i31
                                                                                                                k79)
                                                                                                         ((cps
                                                                                                           py-list?)
                                                                                                          e32
                                                                                                          (lambda (rv80)
                                                                                                            (if rv80
                                                                                                              ((cps
                                                                                                                py-list-ref)
                                                                                                               e32
                                                                                                               i31
                                                                                                               k79)
                                                                                                              ((cps
                                                                                                                tuple?)
                                                                                                               e32
                                                                                                               (lambda (rv81)
                                                                                                                 (if rv81
                                                                                                                   ((cps
                                                                                                                     tuple-ref)
                                                                                                                    e32
                                                                                                                    i31
                                                                                                                    k79)
                                                                                                                   ((cps
                                                                                                                     dict?)
                                                                                                                    e32
                                                                                                                    (lambda (rv82)
                                                                                                                      (if rv82
                                                                                                                        ((cps
                                                                                                                          dict-ref)
                                                                                                                         e32
                                                                                                                         i31
                                                                                                                         k79)
                                                                                                                        (error
                                                                                                                         "cannot index object"
                                                                                                                         k79)))))))))))
                                                                                                       0
                                                                                                       k78))
                                                                                                    t20
                                                                                                    (lambda (rv83)
                                                                                                      ((cps
                                                                                                        dict-set!)
                                                                                                       b26
                                                                                                       i25
                                                                                                       rv83
                                                                                                       k62)))
                                                                                                   (k62
                                                                                                    (void))))))))))))
                                                                                  i
                                                                                  k61))
                                                                               lst
                                                                               (lambda (rv84)
                                                                                 ((lambda (b34
                                                                                           k85)
                                                                                    ((cps
                                                                                      +)
                                                                                     i
                                                                                     1
                                                                                     (lambda (rv108)
                                                                                       ((lambda (i33
                                                                                                 k86)
                                                                                          ((cps
                                                                                            tuple?)
                                                                                           b34
                                                                                           (lambda (rv87)
                                                                                             (if rv87
                                                                                               ((lambda (e36
                                                                                                         k88)
                                                                                                  ((lambda (i35
                                                                                                            k89)
                                                                                                     ((cps
                                                                                                       py-list?)
                                                                                                      e36
                                                                                                      (lambda (rv90)
                                                                                                        (if rv90
                                                                                                          ((cps
                                                                                                            py-list-ref)
                                                                                                           e36
                                                                                                           i35
                                                                                                           k89)
                                                                                                          ((cps
                                                                                                            tuple?)
                                                                                                           e36
                                                                                                           (lambda (rv91)
                                                                                                             (if rv91
                                                                                                               ((cps
                                                                                                                 tuple-ref)
                                                                                                                e36
                                                                                                                i35
                                                                                                                k89)
                                                                                                               ((cps
                                                                                                                 dict?)
                                                                                                                e36
                                                                                                                (lambda (rv92)
                                                                                                                  (if rv92
                                                                                                                    ((cps
                                                                                                                      dict-ref)
                                                                                                                     e36
                                                                                                                     i35
                                                                                                                     k89)
                                                                                                                    (error
                                                                                                                     "cannot index object"
                                                                                                                     k89)))))))))))
                                                                                                   1
                                                                                                   k88))
                                                                                                t20
                                                                                                (lambda (rv93)
                                                                                                  ((cps
                                                                                                    tuple-set!)
                                                                                                   b34
                                                                                                   i33
                                                                                                   rv93
                                                                                                   k86)))
                                                                                               ((cps
                                                                                                 py-list?)
                                                                                                b34
                                                                                                (lambda (rv94)
                                                                                                  (if rv94
                                                                                                    ((lambda (e38
                                                                                                              k95)
                                                                                                       ((lambda (i37
                                                                                                                 k96)
                                                                                                          ((cps
                                                                                                            py-list?)
                                                                                                           e38
                                                                                                           (lambda (rv97)
                                                                                                             (if rv97
                                                                                                               ((cps
                                                                                                                 py-list-ref)
                                                                                                                e38
                                                                                                                i37
                                                                                                                k96)
                                                                                                               ((cps
                                                                                                                 tuple?)
                                                                                                                e38
                                                                                                                (lambda (rv98)
                                                                                                                  (if rv98
                                                                                                                    ((cps
                                                                                                                      tuple-ref)
                                                                                                                     e38
                                                                                                                     i37
                                                                                                                     k96)
                                                                                                                    ((cps
                                                                                                                      dict?)
                                                                                                                     e38
                                                                                                                     (lambda (rv99)
                                                                                                                       (if rv99
                                                                                                                         ((cps
                                                                                                                           dict-ref)
                                                                                                                          e38
                                                                                                                          i37
                                                                                                                          k96)
                                                                                                                         (error
                                                                                                                          "cannot index object"
                                                                                                                          k96)))))))))))
                                                                                                        1
                                                                                                        k95))
                                                                                                     t20
                                                                                                     (lambda (rv100)
                                                                                                       ((cps
                                                                                                         py-list-set!)
                                                                                                        b34
                                                                                                        i33
                                                                                                        rv100
                                                                                                        k86)))
                                                                                                    ((cps
                                                                                                      dict?)
                                                                                                     b34
                                                                                                     (lambda (rv101)
                                                                                                       (if rv101
                                                                                                         ((lambda (e40
                                                                                                                   k102)
                                                                                                            ((lambda (i39
                                                                                                                      k103)
                                                                                                               ((cps
                                                                                                                 py-list?)
                                                                                                                e40
                                                                                                                (lambda (rv104)
                                                                                                                  (if rv104
                                                                                                                    ((cps
                                                                                                                      py-list-ref)
                                                                                                                     e40
                                                                                                                     i39
                                                                                                                     k103)
                                                                                                                    ((cps
                                                                                                                      tuple?)
                                                                                                                     e40
                                                                                                                     (lambda (rv105)
                                                                                                                       (if rv105
                                                                                                                         ((cps
                                                                                                                           tuple-ref)
                                                                                                                          e40
                                                                                                                          i39
                                                                                                                          k103)
                                                                                                                         ((cps
                                                                                                                           dict?)
                                                                                                                          e40
                                                                                                                          (lambda (rv106)
                                                                                                                            (if rv106
                                                                                                                              ((cps
                                                                                                                                dict-ref)
                                                                                                                               e40
                                                                                                                               i39
                                                                                                                               k103)
                                                                                                                              (error
                                                                                                                               "cannot index object"
                                                                                                                               k103)))))))))))
                                                                                                             1
                                                                                                             k102))
                                                                                                          t20
                                                                                                          (lambda (rv107)
                                                                                                            ((cps
                                                                                                              dict-set!)
                                                                                                             b34
                                                                                                             i33
                                                                                                             rv107
                                                                                                             k86)))
                                                                                                         (k86
                                                                                                          (void))))))))))))
                                                                                        rv108
                                                                                        k85))))
                                                                                  lst
                                                                                  k60))))
                                                                            (tuple
                                                                             rv115
                                                                             rv121)
                                                                            k59))))))
                                                                   k44)
                                                                  (k44
                                                                   (void))))))))))
                                                     k43)))
                                                 k42))
                                              k33))))
                                        k32))
                                     k31)))
                                 k30))
                              k18))))))))))
               (lambda (rv122) (return lst k16)))))))
        (void)
        (void)
        k15))
     k14))
  (set-then!
   g$unsort
   (py-list* 30 2 1 4 5 20 3 11 9 31 100 31 3 4 9 10)
   (g$BubbleSort
    g$unsort
    (lambda (rv123)
      (set-then! g$result rv123 ((cps py-print) g$result $halt)))))))
