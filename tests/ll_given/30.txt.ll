(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$s (void))
 (set-then!
  g$s
  "\n"
  (set-then!
   g$s
   "\n"
   (set-then! g$s "\n" (set-then! g$s "\n" (app* $halt (void)))))))
