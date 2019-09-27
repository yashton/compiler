(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$hello (void))
 (set-then!
  g$hello
  "This is a rather long string containing\nseveral lines of text just as you would do in C.\n        Note that leading whitespace is significant."
  (app* $halt (void))))
