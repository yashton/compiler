(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$month (void))
 (set-then!
  g$month
  (py-list*
   "Januari"
   "Februari"
   "Maart"
   "April"
   "Mei"
   "Juni"
   "Juli"
   "Augustus"
   "September"
   "Oktober"
   "November"
   "December")
  (app* $halt (void))))
