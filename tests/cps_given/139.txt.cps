(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  30
  ((cps +)
   g$x
   20
   (lambda (rv14) (set-then! g$x rv14 ((cps py-print) g$x $halt))))))
