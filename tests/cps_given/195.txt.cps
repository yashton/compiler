(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$quicksort (void))
 (define g$partition (void))
 (define g$start (void))
 (define g$end (void))
 (define g$i (void))
 (define g$li (void))
 (set-then!
  g$partition
  (lambda (l start end k14)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k15)
       ((lambda (pivot bottom top done k16)
          ((lambda (e15 k17)
             ((lambda (i14 k18)
                ((cps py-list?)
                 e15
                 (lambda (rv19)
                   (if rv19
                     ((cps py-list-ref) e15 i14 k18)
                     ((cps tuple?)
                      e15
                      (lambda (rv20)
                        (if rv20
                          ((cps tuple-ref) e15 i14 k18)
                          ((cps dict?)
                           e15
                           (lambda (rv21)
                             (if rv21
                               ((cps dict-ref) e15 i14 k18)
                               (error "cannot index object" k18)))))))))))
              end
              k17))
           l
           (lambda (rv22)
             (set-then!
              pivot
              rv22
              ((cps -)
               start
               1
               (lambda (rv23)
                 (set-then!
                  bottom
                  rv23
                  (set-then!
                   top
                   end
                   (set-then!
                    done
                    #f
                    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                     (lambda (break k24)
                       ((lambda (loop k25)
                          (set-then!
                           loop
                           (lambda (k26)
                             ((cps not)
                              done
                              (lambda (rv27)
                                (if rv27
                                  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                   (lambda (continue k28)
                                     ((lambda (k29)
                                        ((lambda (f cc)
                                           (f (lambda (x k) (cc x)) cc))
                                         (lambda (break k30)
                                           ((lambda (loop k31)
                                              (set-then!
                                               loop
                                               (lambda (k32)
                                                 ((cps not)
                                                  done
                                                  (lambda (rv33)
                                                    (if rv33
                                                      ((lambda (f cc)
                                                         (f
                                                          (lambda (x k) (cc x))
                                                          cc))
                                                       (lambda (continue k34)
                                                         ((lambda (k35)
                                                            ((cps +)
                                                             bottom
                                                             1
                                                             (lambda (rv36)
                                                               (set-then!
                                                                bottom
                                                                rv36
                                                                ((lambda (k37)
                                                                   ((cps
                                                                     equal?)
                                                                    bottom
                                                                    top
                                                                    (lambda (rv71)
                                                                      (if rv71
                                                                        ((lambda (k72)
                                                                           (set-then!
                                                                            done
                                                                            1
                                                                            (break
                                                                             k72)))
                                                                         k37)
                                                                        (k37
                                                                         (void))))))
                                                                 (lambda (rv38)
                                                                   ((lambda (e17
                                                                             k39)
                                                                      ((lambda (i16
                                                                                k40)
                                                                         ((cps
                                                                           py-list?)
                                                                          e17
                                                                          (lambda (rv41)
                                                                            (if rv41
                                                                              ((cps
                                                                                py-list-ref)
                                                                               e17
                                                                               i16
                                                                               k40)
                                                                              ((cps
                                                                                tuple?)
                                                                               e17
                                                                               (lambda (rv42)
                                                                                 (if rv42
                                                                                   ((cps
                                                                                     tuple-ref)
                                                                                    e17
                                                                                    i16
                                                                                    k40)
                                                                                   ((cps
                                                                                     dict?)
                                                                                    e17
                                                                                    (lambda (rv43)
                                                                                      (if rv43
                                                                                        ((cps
                                                                                          dict-ref)
                                                                                         e17
                                                                                         i16
                                                                                         k40)
                                                                                        (error
                                                                                         "cannot index object"
                                                                                         k40)))))))))))
                                                                       bottom
                                                                       k39))
                                                                    l
                                                                    (lambda (rv44)
                                                                      ((cps >)
                                                                       rv44
                                                                       pivot
                                                                       (lambda (rv45)
                                                                         (if rv45
                                                                           ((lambda (k46)
                                                                              ((lambda (b19
                                                                                        k47)
                                                                                 ((lambda (i18
                                                                                           k48)
                                                                                    ((cps
                                                                                      tuple?)
                                                                                     b19
                                                                                     (lambda (rv49)
                                                                                       (if rv49
                                                                                         ((lambda (e21
                                                                                                   k50)
                                                                                            ((lambda (i20
                                                                                                      k51)
                                                                                               ((cps
                                                                                                 py-list?)
                                                                                                e21
                                                                                                (lambda (rv52)
                                                                                                  (if rv52
                                                                                                    ((cps
                                                                                                      py-list-ref)
                                                                                                     e21
                                                                                                     i20
                                                                                                     k51)
                                                                                                    ((cps
                                                                                                      tuple?)
                                                                                                     e21
                                                                                                     (lambda (rv53)
                                                                                                       (if rv53
                                                                                                         ((cps
                                                                                                           tuple-ref)
                                                                                                          e21
                                                                                                          i20
                                                                                                          k51)
                                                                                                         ((cps
                                                                                                           dict?)
                                                                                                          e21
                                                                                                          (lambda (rv54)
                                                                                                            (if rv54
                                                                                                              ((cps
                                                                                                                dict-ref)
                                                                                                               e21
                                                                                                               i20
                                                                                                               k51)
                                                                                                              (error
                                                                                                               "cannot index object"
                                                                                                               k51)))))))))))
                                                                                             bottom
                                                                                             k50))
                                                                                          l
                                                                                          (lambda (rv55)
                                                                                            ((cps
                                                                                              tuple-set!)
                                                                                             b19
                                                                                             i18
                                                                                             rv55
                                                                                             k48)))
                                                                                         ((cps
                                                                                           py-list?)
                                                                                          b19
                                                                                          (lambda (rv56)
                                                                                            (if rv56
                                                                                              ((lambda (e23
                                                                                                        k57)
                                                                                                 ((lambda (i22
                                                                                                           k58)
                                                                                                    ((cps
                                                                                                      py-list?)
                                                                                                     e23
                                                                                                     (lambda (rv59)
                                                                                                       (if rv59
                                                                                                         ((cps
                                                                                                           py-list-ref)
                                                                                                          e23
                                                                                                          i22
                                                                                                          k58)
                                                                                                         ((cps
                                                                                                           tuple?)
                                                                                                          e23
                                                                                                          (lambda (rv60)
                                                                                                            (if rv60
                                                                                                              ((cps
                                                                                                                tuple-ref)
                                                                                                               e23
                                                                                                               i22
                                                                                                               k58)
                                                                                                              ((cps
                                                                                                                dict?)
                                                                                                               e23
                                                                                                               (lambda (rv61)
                                                                                                                 (if rv61
                                                                                                                   ((cps
                                                                                                                     dict-ref)
                                                                                                                    e23
                                                                                                                    i22
                                                                                                                    k58)
                                                                                                                   (error
                                                                                                                    "cannot index object"
                                                                                                                    k58)))))))))))
                                                                                                  bottom
                                                                                                  k57))
                                                                                               l
                                                                                               (lambda (rv62)
                                                                                                 ((cps
                                                                                                   py-list-set!)
                                                                                                  b19
                                                                                                  i18
                                                                                                  rv62
                                                                                                  k48)))
                                                                                              ((cps
                                                                                                dict?)
                                                                                               b19
                                                                                               (lambda (rv63)
                                                                                                 (if rv63
                                                                                                   ((lambda (e25
                                                                                                             k64)
                                                                                                      ((lambda (i24
                                                                                                                k65)
                                                                                                         ((cps
                                                                                                           py-list?)
                                                                                                          e25
                                                                                                          (lambda (rv66)
                                                                                                            (if rv66
                                                                                                              ((cps
                                                                                                                py-list-ref)
                                                                                                               e25
                                                                                                               i24
                                                                                                               k65)
                                                                                                              ((cps
                                                                                                                tuple?)
                                                                                                               e25
                                                                                                               (lambda (rv67)
                                                                                                                 (if rv67
                                                                                                                   ((cps
                                                                                                                     tuple-ref)
                                                                                                                    e25
                                                                                                                    i24
                                                                                                                    k65)
                                                                                                                   ((cps
                                                                                                                     dict?)
                                                                                                                    e25
                                                                                                                    (lambda (rv68)
                                                                                                                      (if rv68
                                                                                                                        ((cps
                                                                                                                          dict-ref)
                                                                                                                         e25
                                                                                                                         i24
                                                                                                                         k65)
                                                                                                                        (error
                                                                                                                         "cannot index object"
                                                                                                                         k65)))))))))))
                                                                                                       bottom
                                                                                                       k64))
                                                                                                    l
                                                                                                    (lambda (rv69)
                                                                                                      ((cps
                                                                                                        dict-set!)
                                                                                                       b19
                                                                                                       i18
                                                                                                       rv69
                                                                                                       k48)))
                                                                                                   (k48
                                                                                                    (void))))))))))))
                                                                                  top
                                                                                  k47))
                                                                               l
                                                                               (lambda (rv70)
                                                                                 (break
                                                                                  k46))))
                                                                            k35)
                                                                           (k35
                                                                            (void)))))))))))))
                                                          k34))
                                                       (lambda (rv73)
                                                         (loop k32)))
                                                      (k32 (void))))))
                                               (loop
                                                (lambda (rv74) (k31 (void))))))
                                            (void)
                                            k30))
                                         (lambda (rv75)
                                           ((lambda (f cc)
                                              (f (lambda (x k) (cc x)) cc))
                                            (lambda (break k76)
                                              ((lambda (loop k77)
                                                 (set-then!
                                                  loop
                                                  (lambda (k78)
                                                    ((cps not)
                                                     done
                                                     (lambda (rv79)
                                                       (if rv79
                                                         ((lambda (f cc)
                                                            (f
                                                             (lambda (x k)
                                                               (cc x))
                                                             cc))
                                                          (lambda (continue
                                                                   k80)
                                                            ((lambda (k81)
                                                               ((cps -)
                                                                top
                                                                1
                                                                (lambda (rv82)
                                                                  (set-then!
                                                                   top
                                                                   rv82
                                                                   ((lambda (k83)
                                                                      ((cps
                                                                        equal?)
                                                                       top
                                                                       bottom
                                                                       (lambda (rv117)
                                                                         (if rv117
                                                                           ((lambda (k118)
                                                                              (set-then!
                                                                               done
                                                                               #t
                                                                               (break
                                                                                k118)))
                                                                            k83)
                                                                           (k83
                                                                            (void))))))
                                                                    (lambda (rv84)
                                                                      ((lambda (e27
                                                                                k85)
                                                                         ((lambda (i26
                                                                                   k86)
                                                                            ((cps
                                                                              py-list?)
                                                                             e27
                                                                             (lambda (rv87)
                                                                               (if rv87
                                                                                 ((cps
                                                                                   py-list-ref)
                                                                                  e27
                                                                                  i26
                                                                                  k86)
                                                                                 ((cps
                                                                                   tuple?)
                                                                                  e27
                                                                                  (lambda (rv88)
                                                                                    (if rv88
                                                                                      ((cps
                                                                                        tuple-ref)
                                                                                       e27
                                                                                       i26
                                                                                       k86)
                                                                                      ((cps
                                                                                        dict?)
                                                                                       e27
                                                                                       (lambda (rv89)
                                                                                         (if rv89
                                                                                           ((cps
                                                                                             dict-ref)
                                                                                            e27
                                                                                            i26
                                                                                            k86)
                                                                                           (error
                                                                                            "cannot index object"
                                                                                            k86)))))))))))
                                                                          top
                                                                          k85))
                                                                       l
                                                                       (lambda (rv90)
                                                                         ((cps
                                                                           <)
                                                                          rv90
                                                                          pivot
                                                                          (lambda (rv91)
                                                                            (if rv91
                                                                              ((lambda (k92)
                                                                                 ((lambda (b29
                                                                                           k93)
                                                                                    ((lambda (i28
                                                                                              k94)
                                                                                       ((cps
                                                                                         tuple?)
                                                                                        b29
                                                                                        (lambda (rv95)
                                                                                          (if rv95
                                                                                            ((lambda (e31
                                                                                                      k96)
                                                                                               ((lambda (i30
                                                                                                         k97)
                                                                                                  ((cps
                                                                                                    py-list?)
                                                                                                   e31
                                                                                                   (lambda (rv98)
                                                                                                     (if rv98
                                                                                                       ((cps
                                                                                                         py-list-ref)
                                                                                                        e31
                                                                                                        i30
                                                                                                        k97)
                                                                                                       ((cps
                                                                                                         tuple?)
                                                                                                        e31
                                                                                                        (lambda (rv99)
                                                                                                          (if rv99
                                                                                                            ((cps
                                                                                                              tuple-ref)
                                                                                                             e31
                                                                                                             i30
                                                                                                             k97)
                                                                                                            ((cps
                                                                                                              dict?)
                                                                                                             e31
                                                                                                             (lambda (rv100)
                                                                                                               (if rv100
                                                                                                                 ((cps
                                                                                                                   dict-ref)
                                                                                                                  e31
                                                                                                                  i30
                                                                                                                  k97)
                                                                                                                 (error
                                                                                                                  "cannot index object"
                                                                                                                  k97)))))))))))
                                                                                                top
                                                                                                k96))
                                                                                             l
                                                                                             (lambda (rv101)
                                                                                               ((cps
                                                                                                 tuple-set!)
                                                                                                b29
                                                                                                i28
                                                                                                rv101
                                                                                                k94)))
                                                                                            ((cps
                                                                                              py-list?)
                                                                                             b29
                                                                                             (lambda (rv102)
                                                                                               (if rv102
                                                                                                 ((lambda (e33
                                                                                                           k103)
                                                                                                    ((lambda (i32
                                                                                                              k104)
                                                                                                       ((cps
                                                                                                         py-list?)
                                                                                                        e33
                                                                                                        (lambda (rv105)
                                                                                                          (if rv105
                                                                                                            ((cps
                                                                                                              py-list-ref)
                                                                                                             e33
                                                                                                             i32
                                                                                                             k104)
                                                                                                            ((cps
                                                                                                              tuple?)
                                                                                                             e33
                                                                                                             (lambda (rv106)
                                                                                                               (if rv106
                                                                                                                 ((cps
                                                                                                                   tuple-ref)
                                                                                                                  e33
                                                                                                                  i32
                                                                                                                  k104)
                                                                                                                 ((cps
                                                                                                                   dict?)
                                                                                                                  e33
                                                                                                                  (lambda (rv107)
                                                                                                                    (if rv107
                                                                                                                      ((cps
                                                                                                                        dict-ref)
                                                                                                                       e33
                                                                                                                       i32
                                                                                                                       k104)
                                                                                                                      (error
                                                                                                                       "cannot index object"
                                                                                                                       k104)))))))))))
                                                                                                     top
                                                                                                     k103))
                                                                                                  l
                                                                                                  (lambda (rv108)
                                                                                                    ((cps
                                                                                                      py-list-set!)
                                                                                                     b29
                                                                                                     i28
                                                                                                     rv108
                                                                                                     k94)))
                                                                                                 ((cps
                                                                                                   dict?)
                                                                                                  b29
                                                                                                  (lambda (rv109)
                                                                                                    (if rv109
                                                                                                      ((lambda (e35
                                                                                                                k110)
                                                                                                         ((lambda (i34
                                                                                                                   k111)
                                                                                                            ((cps
                                                                                                              py-list?)
                                                                                                             e35
                                                                                                             (lambda (rv112)
                                                                                                               (if rv112
                                                                                                                 ((cps
                                                                                                                   py-list-ref)
                                                                                                                  e35
                                                                                                                  i34
                                                                                                                  k111)
                                                                                                                 ((cps
                                                                                                                   tuple?)
                                                                                                                  e35
                                                                                                                  (lambda (rv113)
                                                                                                                    (if rv113
                                                                                                                      ((cps
                                                                                                                        tuple-ref)
                                                                                                                       e35
                                                                                                                       i34
                                                                                                                       k111)
                                                                                                                      ((cps
                                                                                                                        dict?)
                                                                                                                       e35
                                                                                                                       (lambda (rv114)
                                                                                                                         (if rv114
                                                                                                                           ((cps
                                                                                                                             dict-ref)
                                                                                                                            e35
                                                                                                                            i34
                                                                                                                            k111)
                                                                                                                           (error
                                                                                                                            "cannot index object"
                                                                                                                            k111)))))))))))
                                                                                                          top
                                                                                                          k110))
                                                                                                       l
                                                                                                       (lambda (rv115)
                                                                                                         ((cps
                                                                                                           dict-set!)
                                                                                                          b29
                                                                                                          i28
                                                                                                          rv115
                                                                                                          k94)))
                                                                                                      (k94
                                                                                                       (void))))))))))))
                                                                                     bottom
                                                                                     k93))
                                                                                  l
                                                                                  (lambda (rv116)
                                                                                    (break
                                                                                     k92))))
                                                                               k81)
                                                                              (k81
                                                                               (void)))))))))))))
                                                             k80))
                                                          (lambda (rv119)
                                                            (loop k78)))
                                                         (k78 (void))))))
                                                  (loop
                                                   (lambda (rv120)
                                                     (k77 (void))))))
                                               (void)
                                               k76))
                                            k29))))
                                      k28))
                                   (lambda (rv121) (loop k26)))
                                  (k26 (void))))))
                           (loop (lambda (rv122) (k25 (void))))))
                        (void)
                        k24))
                     (lambda (rv123)
                       ((lambda (b37 k124)
                          ((lambda (i36 k125)
                             ((cps tuple?)
                              b37
                              (lambda (rv126)
                                (if rv126
                                  ((cps tuple-set!) b37 i36 pivot k125)
                                  ((cps py-list?)
                                   b37
                                   (lambda (rv127)
                                     (if rv127
                                       ((cps py-list-set!) b37 i36 pivot k125)
                                       ((cps dict?)
                                        b37
                                        (lambda (rv128)
                                          (if rv128
                                            ((cps dict-set!)
                                             b37
                                             i36
                                             pivot
                                             k125)
                                            (k125 (void))))))))))))
                           top
                           k124))
                        l
                        (lambda (rv129) (return top k16))))))))))))))
        (void)
        (void)
        (void)
        (void)
        k15))
     k14))
  (set-then!
   g$quicksort
   (lambda (l start end k130)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k131)
        ((lambda (split k132)
           ((cps <)
            start
            end
            (lambda (rv133)
              (if rv133
                ((lambda (k134)
                   (g$partition
                    l
                    start
                    end
                    (lambda (rv135)
                      (set-then!
                       split
                       rv135
                       ((cps -)
                        split
                        1
                        (lambda (rv136)
                          (g$quicksort
                           l
                           start
                           rv136
                           (lambda (rv137)
                             ((cps +)
                              split
                              1
                              (lambda (rv138)
                                (g$quicksort l rv138 end k134)))))))))))
                 k132)
                ((lambda (k139) (return (void) k139)) k132)))))
         (void)
         k131))
      k130))
   (set-then!
    g$start
    0
    (set-then!
     g$end
     8
     (set-then!
      g$li
      (py-list* 4 3 2 10 1 9 7 2 11)
      (g$quicksort
       g$li
       g$start
       g$end
       (lambda (rv140)
         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
          (lambda (break k141)
            ((lambda ($seq14 $loop15 k142)
               ((lambda (k143)
                  ((cps set?)
                   $seq14
                   (lambda (rv145)
                     (if rv145
                       (for-set-k $seq14 $loop15 k143)
                       ((cps tuple?)
                        $seq14
                        (lambda (rv146)
                          (if rv146
                            (for-tuple-k $seq14 $loop15 k143)
                            ((cps py-list?)
                             $seq14
                             (lambda (rv147)
                               (if rv147
                                 (for-py-list-k $seq14 $loop15 k143)
                                 ((cps dict?)
                                  $seq14
                                  (lambda (rv148)
                                    (if rv148
                                      (for-dict-k $seq14 $loop15 k143)
                                      (k143 (void)))))))))))))))
                (lambda (rv144) (k142 (void)))))
             g$li
             (lambda (i38 k149)
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (continue k150)
                  (set-then!
                   g$i
                   i38
                   ((lambda (k151) ((cps py-print) g$i k151)) k150)))
                k149))
             k141))
          $halt)))))))))
