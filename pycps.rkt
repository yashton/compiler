#lang racket
; Primitive operations
(define forop (set 'for-set 'for-py-list 'for-tuple 'for-dict))

(define (forop? expr) (set-member? forop expr))

(define triop (set 'py-list-set! 'dict-set! 'tuple-set!))
		
(define (triop? expr)
	(set-member? triop expr))
			
(define binop (set '< '> 'equal? '>= '<= 'not-equal? 'in? 'not-in? 'eq? 'not-eq? '<< '>> '+ '- '* '/ 'quotient 'modulo 'expt 'assert2 'bitwise-and 'bitwise-or 'bitwise-xor 'py-list-ref	'py-list-remove! 'tuple-ref 'dict-ref 'dict-remove!))
		
(define (binop? expr)
	(set-member? binop expr))
	
(define unop (set 'bitwise-not '+ '- 'integer? 'string? 'tuple? 'dict? 'py-list? 'set? 'assert1 'py-print 'not))
		
(define (unop? expr)
	(set-member? unop expr)) 

(define structs (set 'set 'py-list* 'dict 'tuple))

(define (struct? expr) (set-member? structs expr))
	
(define cps-fun (set 'for-set-k 'for-py-list-k 'for-tuple-k 'for-dict-k))

(define (cps-fun? expr) (set-member? cps-fun expr))	

; primitive-operation? : symbol -> boolean
(define (prim? prim-op)
	(or (triop? prim-op) (binop? prim-op) (unop? prim-op)))

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

; atomic? : expr -> boolean
; atomic? : term -> boolean
(define (aexpr? expr)
	(match expr
		[`(,(or 'lambda 'λ) . ,_)	#t]
		[(or 'call/ec 'call/cc) #t]
		[(? symbol?) #t]
		[(? number?)	 #t]
		[(? string?)	 #t]
		[(? boolean?)	#t]
		['(void)		#t]
		['None		#t]
		['Ellipsis	#t]
		[else		#f]))

; cps-transform-program : cps-transforms a desugared program
(define (cps-transform-program program)
	(match program
		[`(program (define ,a ,b) ... ,body) 
			; =>
			`(program
				,@(map cps-transform-def a b)
				,(T-c body '$halt))]
		[else (error "not a program:" program)]))

; cps-transform-def : cps transforms a variable definition
(define (cps-transform-def var expr)
	`(define ,var ,(M expr)))

; app : construct a call form
; ex: (app 'f 1 2 'x) => '(f 1 2 x)
(define (app f . args) `(f ,@args))

; cont->kont converts a syntactic continuation
;						into a meta-continuation
; ex: (cont->kont '(λ (x) (f x))
;			 =>
;		 (λ (rv) `((λ (x) (f x)) ,rv)
(define (cont->kont q)
	(λ (rv) `(,q ,rv)))

; kont->cont converts a meta-continuation
;						into a syntactic continuation
; ex: (kont->cont k) 
;			 =>
;		`(λ ($rv123) ,(k $rv123))
(define (kont->cont k)
	(let ([$rv (gensym '$rv)])
		`(lambda (,$rv) ,(k $rv))))

; let-cont fakes a let form by immediately
;					applying a lambda form
(define (let-cont v expr body) (list 'let-cont v expr body))

; let-name binds an expression to a value if
;					duplicatin it code bloat code
(define (let-name expr k) (list 'let-name expr k))

; cps-atom converts an atomic value to an atomic cps value
;(define (cps-atom expr) expr)
; M : expr => aexp		 
(define (M aexpr)
	(match aexpr
		[`(lambda (,vars ...) ,body)
			; =>
			(let ([$k (gensym 'k)])
			 `(lambda (,@vars ,$k) 
					,(T-c body $k)))]
		
		[(or 'call/ec 'call/cc)
		 '(lambda (f cc) (f (lambda (x k) (cc x)) cc))]
		
		[(? prim?) `(cps ,aexpr)]
		
		[(? forop?) 
			(match aexpr	
				['for-set 'for-set-k]
				['for-py-list 'for-py-list-k]
				['for-tuple 'for-tuple-k]
				['for-dict 'for-dict-k])]
		
		[(or (? symbol?)		 
				 (? number?)
				 (? string?)
				 (? boolean?)
				'(void)
				'None
				'Ellipsis)				 
			 ; =>
			 aexpr]
		
		[else (error "not an aexpr:" aexpr)]))
		
; cps-transform-k converts an expression to cps and calls
;								 k with an atom holding its return value;
;								 k is a meta-continuation
;(define (cps-transform-k expr k) (list expr k))
; T-k : expr × (aexpr => cexpr) => cexpr
(define (T-k expr k)
	(match expr
		[ (? aexpr?)	(k (M expr))]
		
		[`(begin ,expr)
			(T-k expr k)]
		
		[`(begin ,expr ,exprs ...)
			(T-k expr (λ (_)
									(T-k `(begin ,@exprs) k)))]
		
		[`(if ,exprc ,exprt ,exprf)
			; We have to reify the cont to avoid
			; a possible code blow-up:
			(define $rv (gensym 'rv))
			(define cont `(lambda (,$rv) ,(k $rv)))
			(T-k exprc (λ (aexp)
					 `(if ,aexp 
								,(T-c exprt cont)
								,(T-c exprf cont))))]
			
		[`(set! ,var ,expr)
			; =>
			(T-k expr (λ (aexp)
									`(set-then! ,var ,aexp
															,(k '(void)))))]
		
		[`(dict (,ks ,vs) ...)
			; =>
			(T*-k ks (λ (k-aexps)
										(T*-k vs (λ (v-aexps)
																	(k `(dict ,@(map list k-aexps v-aexps)))))))]
		
		[`(,(and op (? struct?)) ,items ...)
			; =>
				(T*-k items (λ (aexps)
									(k `(,op ,@aexps))))]
		
		[`(,_ ,_ ...)
			; =>
			(define $rv (gensym 'rv))
			(define cont `(lambda (,$rv) ,(k $rv)))
			(T-c expr cont)]	
						
		[else (error "no match in T-k:" expr)]))
			
; cps-transform-k* : exp list (exp list -> answer) -> answer
;(define (cps-transform-k* exps k) (list exps k)) 
(define (T*-k exprs k)
	(cond
		[(null? exprs)	 (k '())]
		[(pair? exprs)	 (T-k (car exprs) (λ (hd)
											 (T*-k (cdr exprs) (λ (tl)
												 (k (cons hd tl))))))]))

; cps-transform-q converts an expression to cps and
;								 inserts a call to q with its return value
;								 q is a syntactic continuation
;(define (cps-transform-q exp q) (list exp q))
; T-c : expr × aexp => cexp
(define (T-c expr c)
	(match expr
		[ (? aexpr?)	`(,c ,(M expr))]
		
		[`(begin ,expr)			(T-c expr c)]
		
		[`(begin ,expr ,exprs ...)
			(T-k expr (λ (_)
									(T-c `(begin ,@exprs) c)))]
		
		[`(if ,exprc ,exprt ,exprf)
			; We have to bind the cont to avoid
			; a possible code blow-up:
;			(define $k (gensym 'k))
;			`((lambda (,$k)
;					,(T-k exprc (λ (aexp)
;												`(if ,aexp 
;														 ,(T-c exprt $k)
;														 ,(T-c exprf $k)))))
;				,c)]
			(T-k exprc (λ (aexp)
												`(if ,aexp 
														 ,(T-c exprt c)
														 ,(T-c exprf c))))]
					 
		[`(set! ,var ,expr)
			(T-k expr (λ (aexp)
									`(set-then! ,var ,aexp
															(,c (void)))))]

		[`(,(and op (? struct?)) ,_ ...)
			; =>
				(T-k expr (cont->kont c))]

		[`(,f ,es ...)		
			; =>
			(T-k f (λ ($f)
						 (T*-k es (λ ($es)
											`(,$f ,@$es ,c)))))]
											
		[else (error "no match in T-c:" expr)]))

;; Extensions

(define (cps f)
	(λ args
		(match args
			[`(,xs ... ,k)
			 (k (apply f xs))])))


(define-syntax set-then!
	(syntax-rules ()
		[(_ var exp then)
		 (begin
			 (set! var exp)
			 then)]))

; desugar call/cc or call/ec -> (λ (f cc) (f (λ (x _) (cc x)) cc)) 

(pretty-write (cps-transform-program (read)))
