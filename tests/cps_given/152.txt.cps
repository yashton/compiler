(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$main (void))
 (define g$HOST (void))
 (define g$print_result (void))
 (define g$get_result_from_server (void))
 (set-then!
  g$HOST
  "http://matt.might.net/apps/pyparse/pyparse.php"
  (set-then!
   g$get_result_from_server
   (lambda (file_contents k14)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k15)
        ((lambda (params result k16)
           (set-then!
            params
            (dict)
            ((lambda (b15 k17)
               ((lambda (i14 k18)
                  ((cps tuple?)
                   b15
                   (lambda (rv19)
                     (if rv19
                       ((cps tuple-set!) b15 i14 file_contents k18)
                       ((cps py-list?)
                        b15
                        (lambda (rv20)
                          (if rv20
                            ((cps py-list-set!) b15 i14 file_contents k18)
                            ((cps dict?)
                             b15
                             (lambda (rv21)
                               (if rv21
                                 ((cps dict-set!) b15 i14 file_contents k18)
                                 (k18 (void))))))))))))
                "file"
                k17))
             params
             (lambda (rv22)
               ((lambda (b17 k23)
                  ((lambda (i16 k24)
                     ((cps tuple?)
                      b17
                      (lambda (rv25)
                        (if rv25
                          ((cps tuple-set!) b17 i16 1 k24)
                          ((cps py-list?)
                           b17
                           (lambda (rv26)
                             (if rv26
                               ((cps py-list-set!) b17 i16 1 k24)
                               ((cps dict?)
                                b17
                                (lambda (rv27)
                                  (if rv27
                                    ((cps dict-set!) b17 i16 1 k24)
                                    (k24 (void))))))))))))
                   "lex"
                   k23))
                params
                (lambda (rv28)
                  (get-field
                   g$urllib
                   urlencode
                   (lambda (rv29)
                     (rv29
                      params
                      (lambda (rv30)
                        (set-then!
                         params
                         rv30
                         (get-field
                          g$urllib
                          urlopen
                          (lambda (rv31)
                            (rv31
                             g$HOST
                             params
                             (lambda (rv32)
                               (set-then!
                                result
                                rv32
                                (return result k16)))))))))))))))))
         (void)
         (void)
         k15))
      k14))
   (set-then!
    g$print_result
    (lambda (sin k33)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k34)
         ((lambda (code k35)
            (get-field
             sin
             read
             (lambda (rv36)
               (rv36
                (lambda (rv37)
                  (set-then!
                   code
                   rv37
                   (get-field
                    g$sys
                    stdout
                    (lambda (rv38)
                      (get-field
                       rv38
                       write
                       (lambda (rv39)
                         (g$get_result_from_server
                          code
                          (lambda (rv40)
                            (get-field
                             rv40
                             read
                             (lambda (rv41)
                               (rv41
                                (lambda (rv42) (rv39 rv42 k35)))))))))))))))))
          (void)
          k34))
       k33))
    (set-then!
     g$main
     (lambda (argv k43)
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (return k44) ((lambda (k45) (g$print_result argv k45)) k44))
        k43))
     ((cps equal?)
      g$__name__
      "__main__"
      (lambda (rv46)
        (if rv46
          ((lambda (k47)
             (get-field g$sys stdin (lambda (rv48) (g$main rv48 k47))))
           $halt)
          ($halt (void))))))))))
