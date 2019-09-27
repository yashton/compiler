(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  "escape test \\ ' \" \a \b \f \n \r \t \v"
  (app* $halt (void))))
