#lang racket

;; Helpers.

; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
	(if (not (pair? lst))
	 (k '() '())
	 (partition-k pred (cdr lst) (λ (in out)
		 (if (pred (car lst))
			(k (cons (car lst) in) out)
			(k in (cons (car lst) out)))))))

; define? : term -> boolean
(define (define? sx)
	(match sx
		[`(define . ,_) #t]
		[else		 #f]))

; not-define? : term -> boolean
(define (not-define? sx)
	(not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
	(match exp
		[`(,(or 'lambda 'λ) . ,_)	#t]
		[(? number?)	 #t]
		[(? string?)	 #t]
		[(? boolean?)	#t]
		[`(quote . ,_) #t]
		['(void)		#t]
		['None		#t]
		['Ellipsis	#t]
		[else		#f]))
		
(define (triop? exp)
	(match exp
		['py-list-set! #t]
		['dict-set! #t]
		['tuple-set! #t]
		[else #f]))
		
(define (binop? exp)
	(match exp
		['< #t]
		['> #t]
		['equal? #t]
		['>= #t]
		['<= #t]
		['not-equal? #t]
		['in? #t]
		['not-in? #t]
		['eq? #t]
		['not-eq? #t]
		['<< #t]
		['>> #t]
		['+ #t]
		['-	#t]
		['* #t]
		['/ #t]
		['quotient #t]
		['modulo #t]
		['expt #t]
		['assert2 #t]
		['for-set #t]
		['for-py-list #t]
		['for-tuple #t]
		['for-dict #t]
		['bitwise-and #t]
		['bitwise-or #t]
		['bitwise-xor #t]
		['py-list-ref	#t]
		['py-list-remove! #t]
		['tuple-ref #t]
		['dict-ref #t]
		['dict-remove! #t]
		[else #f]))
		
(define (unop? exp)
	(match exp
		['bitwise-not #t]
		['+ #t]
		['- #t]
		['integer? #t]
		['string? #t]
		['tuple? #t]
		['dict? #t]
		['py-list? #t]
		['set? #t]
		['assert1 #t]
		['py-print #t]
		['not #t]
		[else #f]))
		

; atomic-define? : term -> boolean
(define (atomic-define? def)
	(match def
		[`(define ,v ,exp)	(atomic? exp)]
		[else			#f]))

; global-name : symbol -> symbol
(define (global-name name)
	(string->symbol (string-append "g$" (symbol->string name))))

; atomize-tops : top list -> top list
(define (atomize-tops tops)
	(match tops
		['()	#;=>	'()]
	
		[(cons (and head (? atomic-define?)) tail)
			; =>
			(cons head (atomize-tops tail))]
	
		[(cons `(define ,v ,exp) tail)
			; =>
			`((define ,v (void))
				(set! ,v ,exp)
				,@(atomize-tops tail))]
	
		[(cons head tail) (cons head (atomize-tops tail))]))

;; Desugaring.

; desugar-top : top -> top
(define (desugar-top top)
	(match top
		[`(define ,v ,exp)	`(define ,(global-name v) ,(desugar-exp exp))]
		[exp (desugar-exp exp)]))
	
	
; desugar : program -> program
(define (desugar-program program)
	
	(define prog (match program [`(program . ,stmts) stmts]))
	
	(set! prog (atomize-tops prog))
	
	(set! prog (map desugar-top prog))
	
	(set! prog (append (list
					'(define break (void))
					'(define return (void))
					'(define continue (void))
					'(define $current-handler (void)))
				 prog))
	
	(set! prog
	(partition-k 
		atomic-define?
		prog
		(λ (atomic complex)
			(append atomic `((begin ,@complex))))))
	
		`(program ,@prog))


; desugar-exp : exp -> exp
(define (desugar-exp exp)
	(match exp
		[(? symbol?) exp]
		
		[`(let ((,vs ,es) ...) . ,body)
			`((lambda ,vs (begin ,@(desugar-exp body))) ,@(map desugar-exp es))]
			
		; Advanced let forms not in spec, provided for convenience in reapplication
;		[`(letrec ((,vs ,es) ...) . ,body)
;			(error "haven't handled letrec")]

		[`(let* () ,body)
			(desugar-exp body)]
;	
		[`(let* ((,v ,e) . ,rest) ,body)
			(desugar-exp `(let ((,v ,e)) (let* (,@rest) ,body)))]
	
;		[`(quote ,_) (error "quotes not allowed in hir")]

		[`(,(or 'lambda 'λ) ,params ,body)
			`(lambda ,params ,(desugar-exp body))]

		[`(call/ec ,exp)
			`(call/ec ,(desugar-exp exp))]

		[`(cond) '(void)]
	
		[`(cond (else ,exp))
			(desugar-exp exp)]
	
		[`(cond (,test ,exp))
			`(if ,(desugar-exp test)
					,(desugar-exp exp)
					(void))]
	
		[`(cond (,test ,exp) ,rest ...)
			`(if ,(desugar-exp test) 
					,(desugar-exp exp)
					,(desugar-exp `(cond ,@rest)))]
	
		[`(and)	 #t]

		[`(or)	#f]
	
		[`(or ,exp) (desugar-exp exp)]
	
		[`(and ,exp) (desugar-exp exp)]
	
		; The two reductions differ from the spec, as the spec uses the first for
		; or and the second for and. They are semantically equivalent and I've
		; chosen the if form.
;		[`(or ,exp . ,rest)
;			`(if ,(desugar-exp exp) #t ,(desugar-exp `(or ,@rest)))]
		[`(or ,exp . ,rest)
			(let ([var (gensym 't)])
				(desugar-exp `(let ((,var ,exp)) (if ,var ,var (or ,@rest)))))]
	
		[`(and ,exp . ,rest)
			`(if ,(desugar-exp exp) ,(desugar-exp `(and ,@rest)) #f)]		
;		[`(and ,exp . ,rest)
;			(let ([var (gensym 't)])
;				(desugar-exp `(let ((,var ,exp)) (if ,var (and ,@rest) ,var))))] 

		[`(if ,test ,exp)
				(desugar-exp `(if ,test ,exp (void)))]

		[`(if ,test ,exp1 ,exp2)
			`(if ,(desugar-exp test)
					,(desugar-exp exp1)
					,(desugar-exp exp2))]
	
		[`(set! ,v ,exp)
			`(set! ,v ,(desugar-exp exp))]

		[`(assert ,test)
			(desugar-exp `(assert1 ,test))]
				
		[`(assert ,test ,kind)
			(desugar-exp `(assert2 ,test ,kind))]
				
		[`(get-global ,var)
			(global-name var)]
	
		[`(set-global! ,var ,exp)
			(desugar-exp `(set! ,(global-name var) ,exp))]
	
		[`(begin . ,exps)
			`(begin ,@(map desugar-exp exps))]		
	
		['(return) '(return)]
	
		['(break) '(break)]

		['(continue) '(continue)]

		[`(while ,cond ,body)
			(desugar-exp `(while ,cond ,body (void)))]
	
		[`(while ,cond ,body ,else)
			(desugar-exp
				`(call/ec
					(lambda (break)
						(let ((loop (void)))
								(set! loop
									(lambda ()
										(if ,cond
											(begin
												(call/ec
													(lambda (continue) ,body))
												(loop)))))
								(loop)
								,else))))]
;			`(call/ec
;				(lambda (break)
;					((lambda (loop)
;						(begin
;							(set! loop
;								(lambda ()
;									(if ,(desugar-exp cond)
;										(begin
;											(call/ec
;												(lambda (continue)
;													,(desugar-exp body)))
;											(loop))
;										(void))))
;							(loop)
;							,(desugar-exp else)))
;					(void))))]
		
		[`(for-each ,var ,items ,body) 
			(desugar-exp `(for-each ,var ,items ,body (void)))]
		
		[`(for-each ,var ,items ,body ,else)
			(let ([seq (gensym '$seq)] [loop (gensym '$loop)])
				(desugar-exp 
					`(call/ec
						(lambda (break)
							(let ((,seq ,items)
										(,loop (lambda (,var)
															(call/ec
																(lambda (continue) ,body)))))
										(begin
											(cond ((set? ,seq) (for-set ,seq ,loop))
														((tuple? ,seq) (for-tuple ,seq ,loop))
														((py-list? ,seq) (for-py-list ,seq ,loop))
														((dict? ,seq)	(for-dict ,seq ,loop)))
											,else))))))]
;				`(call/ec
;					(lambda (break)
;						((lambda (,seq ,loop)
;							(begin
;								(begin
;									(if (set? ,seq)
;										(for-set ,seq ,loop)
;										(if (tuple? ,seq)
;											(for-tuple ,seq ,loop)
;											(if (py-list? ,seq)
;												(for-py-list ,seq ,loop)
;												(if (dict? ,seq)
;													(for-dict ,seq ,loop)
;													(void)))))
;									,(desugar-exp else))))
;						,(desugar-exp items)
;						(lambda (,var)
;							(call/ec
;								(lambda (continue)
;									,(desugar-exp body))))))))]
		
		[`(dict (,keys ,values) ...)
			`(dict 
				,@(map 
					(lambda (k v) (list (desugar-exp k) (desugar-exp v))) 
					keys values))]
	
		[`(set . ,values)
			`(set ,@(map desugar-exp values))]		
	
		[`(tuple . ,values)
			`(tuple ,@(map desugar-exp values))]
	
		[`(py-list* . ,values)
			`(py-list* ,@(map desugar-exp values))]
	
		[`(try ,body ,handler)
			(let ([ec (gensym '$ec)] [ex (gensym '$ex)])
				(desugar-exp 
					`(let* ([$old-handler $current-handler]
									[$old-return return]
									[$old-continue continue]
									[$old-break break])
						(let* ([return (lambda (rv) 
														(begin
															(set! $current-handler $old-handler)
															(return rv)))]
									[continue (lambda ()
															(begin
															 (set! $current-handler $old-handler)
															 ($old-continue)))]
									[break	(lambda ()
														(begin
															(set! $current-handler $old-handler)
															($old-break)))])
							 (call/ec 
								 (lambda (,ec)
								 		(begin
											(set! $current-handler 
												(lambda (,ex)
															 (begin
															 	(set! $current-handler $old-handler)
																(,ec (,handler ,ex)))))
											(let ([rv ,body])
													(begin 
														(set! $current-handler $old-handler)
														rv)))))))))]
	
		[`(throw ,exp) `($current-handler ,(desugar-exp exp))]
	
		[(? atomic?)	 exp]

		[`(,f . ,args)	
			(let
			 ([func (match f
							[(? triop?) f]
							[(? binop?) f]
							[(? unop?) f]
							[else (desugar-exp f)])])
				`(,func ,@(map desugar-exp args)))]
			
;		[else exp]))
		[else	(error (format "desugar fail: ~s~n" exp))]))




(pretty-write (desugar-program (read)))


	 
