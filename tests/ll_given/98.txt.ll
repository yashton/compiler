(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$b (void))
 (define g$c (void))
 (set-then! g$a 1 (set-then! g$b 2 (set-then! g$c 3 (app* $halt (void))))))
