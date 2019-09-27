(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$hello (void))
 (set-then!
  g$hello
  "This is a rather long string containing\\n\\\n    several lines of text much as you would do in C."
  (app* $halt (void))))
