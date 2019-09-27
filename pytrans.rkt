#lang racket

; Ashton Snelgrove
; u0662114
; snelgrov@eng.utah.edu

(require srfi/41) ; Stream library
(require test-engine/racket-tests)
(require racket/pretty)

; environments are sets of symbols:
(define empty-env (set))

; transforms a program:
(define (transform-program program)
 (match program
   [`(program . ,(app flatten-stmts stmts))
    ; =>
    (define globals (local-bindings stmts))
    `(program
      ,@(for/list ([g globals]) `(define ,g (void)))
      ,@(map (λ (s) (transform-stmt empty-env s)) stmts))]

   [else (error (format "not a program: ~s~n" program))]))
   

; flattens internal compound statements into the outer block:
(define (flatten-stmts stmts) stmts)

; converts an augment assignment operator to the target binop:
(define (select-augassign op) op)

; determines global bindings from a group of statements:
(define (global-bindings stmts) stmts)

; finds all of the bindings in a list of l-values:
(define (lvals->bindings lvals) lvals)

;; finds all of the local bindings in a list of statements:
(define (local-bindings stmts) 
	(match stmts
		[`((= ,vars ,expr) ,rest ...) 
			(set-union 
				(set-add-list (set)
					(filter (λ (s) (match s [`(indexed ,_ ...) #f] [else #t])) vars))
				(local-bindings rest))]
		[`((def ,sig ,body) ,rest ...) 
			(set-union (set-add (set) (car sig)) (local-bindings rest))]
		[`((suite ,stmts ...) ,rest ...)
			(set-union (local-bindings stmts) (local-bindings rest))]
		[`((while ,cond ,suite) ,rest ...)
			(set-union (local-bindings (suite->stmts suite)) (local-bindings rest))]
		[`((,(regexp #rx"([-+*/%&|^]|<<|>>|~*~*|//)=" op) ,vars ,expr) ,rest ...)
				(set-union 
					(set-add-list (set) 
							(filter (λ (s) (match s [`(indexed ,_ ...) #f] [else #t])) vars))
					(local-bindings rest))]  
		[`((for ,name ,test ,suites ...) ,rest ...)
			; =>
			(set-union 
				(set-add (set) name) 
				(foldl set-union (set) (map (λ (s) (local-bindings (suite->stmts s))) suites))
				(local-bindings rest))]
		[`((try ,body ,except) ,rest ...) 
			; =>
			(set-union
				(local-bindings (suite->stmts body)) (local-bindings rest))]
		[`((cond (,conds ,suites) ...) ,rest ...)
			; =>
			(set-union
				(foldl set-union (set) (map (λ (s) (local-bindings (suite->stmts s))) suites))
				(local-bindings rest))]
		[`((cond (,conds ,suites) ... (else ,suite)) ,rest ...)
			; =>
			(set-union				
				(foldl set-union (set) (map (λ (s) (local-bindings (suite->stmts s))) suites))
				(local-bindings (suite->stmts suite))
				(local-bindings rest))]
		[`((,a ...) ,c ...) (local-bindings c)]
		[`((,a ...)) (set)]
		[`() (set)]
		[else (error "unknown in local-bindings for:" stmts)]))

; generates code to set an indexed l-value:
(define (set-index env $base index $value)
	(let ([b (gensym 'b)]
				[i (gensym 'i)]
				[index (transform-test env index)]
				[v (gensym 'v)])
		`(let ((,b ,$base))
			(let ((,i ,index))
				(let ((,v ,$value))
					(cond
						((tuple? ,b) (tuple-set! ,b ,i ,v))
						((py-list? ,b) (py-list-set! ,b ,i ,v))
						((dict? ,b) (dict-set! ,b ,i ,v))))))))

; generates code to delete an indexed l-value:
(define (delete-index env $base index) (list $base index))

(define (aug-op-type op)
	(match op
		[(or "-" "+") 'arith]
		[(or "*" "/" "%" "//") 'term]
		[(or "<<" ">>") 'shift]
		["&" 'bitwise-and]
		["^" 'bitwise-xor]
		["|" 'bitwise-or]
		["**" 'power]
		[else (error "unknown augassign operation:" op)]))

(define (accum t) (λ () (set! t (+ t 1)) (- t 1)))

;; generates code for a statement:
(define (transform-stmt env stmt)
	(match stmt
		[`(begin ,stmts ...) `(begin ,(map (transform-stmt-with env) stmts))]
		[`(= ,lhs ,rhs) 
			; =>
			(let ([r (transform-test env rhs)]) 
				(if (> (length lhs) 1)
					(let ([expr (gensym 't)]
								[acc (accum 0)])
						(let ([new-env (set-add env expr)])
						; result
						`(let ((,expr ,r))
								,@(for/list ([var lhs])
										(transform-stmt new-env 
											`(= (,var) (indexed ,expr (subscript ,(acc)))))))))
					(let ([l (transform-test env (car lhs))])
						(match l
							;[`(tuple ,a ...) `(tuple-set! ,l ,r)]
							[(? (λ (m) (set-member? env m))) `(set! ,l ,r)]
							[`(get-global ,g) `(set-global! ,g ,r)]
							[else (match (car lhs)
											[`(indexed ,a ,b ... (subscript ,c))
												; =>
												(set-index env (transform-expr env `(indexed ,a ,@b)) c r)]
											[`(indexed ,a (subscript ,c))
												; =>
												(set-index env (transform-atom env a) c r)]
											[else (error "unknown left hand side:" `(= ,lhs ,rhs))])]))))]
		[`(,(regexp #rx"([-+*/%]|<<|>>|//)=" op) (,lhs) ,rhs) 
			; =>
			(transform-stmt env 
				`(= (,lhs) (,(aug-op-type (second op)) ,lhs (,(second op) ,rhs))))]
		[`(,(regexp #rx"([&|^]|~*~*)=" op) (,lhs) ,rhs) 
			; =>
			(transform-stmt env 
				`(= (,lhs) (,(aug-op-type (second op)) ,lhs ,rhs)))]
		[`(expr ,t) (transform-test env t)] 		
		[`(del ,star) `(del (transform-star env star))]
		[`(pass) `(pass)]
		[`(break) `(break)]
		[`(continue) `(continue)]
		[`(raise ,test) `(throw (transform-test env test))]
		[`(return ,test ...) `(return ,@(map (transform-test-with env) test))] 
		[`(raise ,test ...) `(raise ,@(map (transform-test-with env) test))]
		[`(global ,names ...) '(void)]
		[`(nonlocal ,names ...) '(void)]
		[`(assert ,tests ...) `(assert ,@(map (transform-test-with env) tests))]
		[`(cond (,tests ,suites) ... (else ,end)) 
			; =>
			`(cond 
				,@(map (λ (test suite)	
								(list (transform-test env test) (transform-suite env suite)))
						tests	suites) 
					(else ,(transform-suite env end)))]
		[`(cond (,tests ,suites) ...)
			; =>
				`(cond 
					,@(map (λ (test suite) 
							(list (transform-test env test) (transform-suite env suite)))
						tests suites))]
		[`(while ,test ,suites ...) 
			; =>
			`(while ,(transform-test env test) 
				,@(map (λ (suite) (transform-suite env suite)) suites))]
		[`(for ,name ,test ,body ...)
			; =>
			(let ([var (gensym 'i)])
				(let ([envnew (set-add env var)])
					`(for-each ,var ,(transform-test env test) 
							(begin
								,(if (set-member? envnew name)
									`(set! ,name ,var)
									`(set-global! ,name ,var)) 
								,@(map (λ (suite) (transform-suite envnew suite)) body)))))]
		[`(try ,a ,b) `(try ,(transform-suite env a) ,(transform-suite b))]
		[`(def (,name ,vars ...) ,body) 
			; =>
			(let ([new-env (set-add-list env vars)])
				`(set-global! ,name
						(lambda ,vars
						(call/ec (lambda (return) ,(transform-body-suite new-env body))))))] 
		
	;	[else stmt])) 
		[else (error "unknown in transform-stmt for:" env stmt)]))

(define (set-add-list s l)
	(if (equal? '() l)
			s
			(set-add-list (set-add s (car l)) (cdr l))))

; a curried form of transform-stmt, useful in conjuction with map:
(define (transform-stmt-with env)
	(λ (s) (transform-stmt env s)))

	
; transform test expressions
(define (transform-test env test) 
	(match test
		[`(tuple ,tests ...) `(tuple ,@(map (transform-test-with env) tests))]
		[`(if ,a ,b ,c) 
			;=>
			`(if ,(transform-test env a) 
					,(transform-test env b)
					 (transform-test env c))]
		[`(lambda ,names ,tests)
			;=>
			`(lambda ,names ,@(map (transform-test-with env) tests))] 
		[`(or ,a ...) `(or ,@(map (transform-test-with env) a))]
		[`(and ,a ...) `(and ,@(map (transform-test-with env) a))]
		[`(not ,a) `(not ,(transform-test env a))]
		[`(comparison ,star (,cmp ,expr))
			; =>
			`(,(select-cmp cmp) ,(transform-star env star) ,(transform-star env expr))] 
		[`(comparison ,star (,cmp ,expr) (,cmpb ,exprb))
			; =>
			(let ([var (gensym 'cv)])
				; result
				`(let ((,var ,(transform-expr env expr))) 
					(if (,(select-cmp cmp) ,(transform-star env star) ,var)
						(,(select-cmp cmpb) ,var ,(transform-expr env exprb))
						#f)))] 
		[else (transform-star env test)]))

(define (transform-star env star)
	(match star
		[`(star ,expr) (star (transform-expr env expr))]
		[else (transform-expr env star)]))

; curried transform-test
(define (transform-test-with env)
	(λ (s) (transform-test env s)))
	
; selects the HIR comparison op given the Python op:
(define (select-cmp cmp) 
	(match cmp
		["<" '<]
		[">" '>]
		[">=" '>=]
		["<=" '<=]
		["==" 'equal?]
		["!=" 'not-equal?]
		["in" 'in?]
		["not-in" 'not-in?]
		["is" 'is?]
		["is-not" 'is-not?]
		[else (error "comparison not valid:" cmp)]))

; selects the HIR shift op given the Python op:
(define (select-shift op) 
	(match op
		["<<" '<<]
		[">>" '>>]
		[else (error "unknown shift op:" op)]))

; selects the HIR arithmetic op given the Python op:
(define (select-arith op) 
	(match op
		["+" '+]
		["-" '-]
		[else (error "unknown arith op:" op)]))

; selects the HIR term op given the Python op:
(define (select-term op) 
	(match op
		["*" '*]
		["/" '/]
		["%" 'modulo]
		["//" 'quotient]
		[else (error "uknown term op:" op)]))

; selects the HIR unary op given the Python op:
(define (select-unary op) 
	(match op
		["+" '+]
		["-" '-]
		["~" 'bitwise-not]
		[else (error "uknown unary op:" op)]))

; unfolds a comparison exp in Python into an HIR exp:
(define (unwind-comparison env expr ops) (list expr ops))

; unfolds a binary op exp in Python into an HIR exp:
(define (unwind-op select-op env $expr ops) (list select-op env $expr ops))

; unfolds a trailer in Python into an HIR exp:
(define (unwind-trailer env $expr trailer)
	(match trailer
		[`(called) `(,$expr)]
		[`(called ,args ...) `(,$expr ,@(map (λ (i) (transform-test env i)) args))]
		[`(subscript (tuple ,tests ...)) 
			; =>
			`(subscript ,$expr (tuple ,@(map (λ (i) (transform-test env i)) tests)))]
		[`(subscript ,test)
			; =>
			(let ([var (gensym 'e)])
				(let ([index (gensym 'i)])
				; result
				`(let ((,var ,$expr)) 
					(let ((,index ,(transform-test env test)))
						(cond
							((py-list? ,var) (py-list-ref ,var ,index))
							((tuple? ,var) (tuple-ref ,var ,index))
							((dict? ,var) (dict-ref ,var ,index))
							(else (error "cannot index object")))))))]
		[`(dot ,name) `(get-field ,$expr ,name)]
		[else (error "unknown trailer:" trailer)]))

; unfolds a sequence of trailers into an HIR exp:
(define (unwind-trailers env $expr trailers) 
	(if (equal? trailers '())
		$expr
		(let ([$new (unwind-trailer env $expr (car trailers))])
			(unwind-trailers env $new (cdr trailers)))))

; transforms a Python exp into an HIR exp:
(define (transform-expr env expr) 
	(match expr
		[`(bitwise-or ,exprs ...)
			; =>
			`(bitwise-or ,@(map (λ (expr) (transform-expr env expr)) exprs))]
		[`(bitwise-xor ,exprs ...)
			; =>
			`(bitwise-xor ,@(map (λ (expr) (transform-expr env expr)) exprs))]
		[`(bitwise-and ,exprs ...)
			; =>
			`(bitwise-and ,@(map (λ (expr) (transform-expr env expr)) exprs))]
		[`(shift ,ex (,op ,ar))
			; =>
			`(,(select-shift op) ,(transform-expr env ex) ,(transform-expr env ar))]
		[`(shift ,expr (,op ,ar) ,rest ...)
			; =>
			`(,(select-shift op) ,(transform-expr env expr) 
					,(transform-expr env `(shift ,ar ,@rest)))]
		[`(arith ,a (,op ,b))
			; =>
			`(,(select-arith op) ,(transform-expr env a) ,(transform-expr env b))]
		[`(term ,a ,rest ...) (unwind-term env (transform-expr env a) rest)]
		[`(power ,indexed ,factor)
			; =>
			`(expt ,(transform-expr env indexed) ,(transform-expr env factor))]
		[`(indexed ,atom ,trailer) (unwind-trailer env (transform-atom env atom) trailer)]
		[`(indexed ,atom ,trailers ...) (unwind-trailers env (transform-atom env atom) trailers)] 
		[`(,op ,expr) `(,(select-unary op) ,(transform-expr env expr))]
		[else (transform-atom env expr)])) 
		
		
(define (unwind-term env first rest)
	(match rest
		[`((,op ,t) ,rest ...) 
			; =>
			(unwind-term env `(,(select-term op) ,first ,(transform-expr env t)) rest)]
		[`((,op ,t))
			; =>
			`(,(select-term op) ,first ,(transform-expr env t))]
		['() first]
		[else (error "weird stuff in unwind-term" `(,first ,rest))]))
		
(define (transform-atom env atom)
	(match atom
		[`(list ,items ...) `(py-list* ,@(map (λ (i) (transform-test env i)) items))]
		[`(dict ,pairs ...)	
			; =>
				`(dict ,@(map (λ (p) (map (λ (i) (transform-test env i)) p)) pairs))]
		[`(set ,items ...) `(set ,@(map (λ (i) (transform-test env i)) items))]
		[`(tuple) `(tuple)]
		[`(tuple ,items ...) `(tuple ,@(map (λ (i) (transform-test env i)) items))]
		[(? number? atom) atom]
		[(? string? atom) atom]
		['Ellipsis 'Ellipsis]
		['None 'None]
		['True #t]
		['False #f]
		['print 'py-print]
		;[(regexp #rx"[_a-zA-Z][_a-zA-Z0-9]*") atom]
		[`(,a) (transform-test env atom)]
		[else (if (set-member? env atom)
						atom
						`(get-global ,atom))]))
		
; transform a suite into a list of statements:
(define (suite->stmts suite) 
	(match suite
		[`(suite ,stmts ...) stmts]
		[else `(,suite)]))

; transform a suite that begins a new scope (function body):
(define (transform-body-suite env suite) 
	(let ([stmts (suite->stmts suite)])
		(let ([vars (local-bindings stmts)])
			(let ([new-env (set-union env vars)])
				; result
				`(let (,@(for/list ([v vars]) `(,v (void))))
					,@(map (transform-stmt-with new-env) stmts))))))
;0. Flattens the statements.
;1. Finds new local bindings.
;2. Creates a let-form for these.
;3. Updates the environment.
;4. Transforms all of the statements into the body of the let. 
		
; transform a suite that does not begin a new scope:
(define (transform-suite env suite) 
	(let ([stmts (suite->stmts suite)])
		`(let () ,@(map (transform-stmt-with env) stmts))))

; Execute
(define (python-trans-file filename)
  (python-trans-port (open-input-file filename)))
     
(define (python-trans-port port)
  (pretty-write (transform-program (read port))))
      
(match (current-command-line-arguments)
  [(vector filename)
   ; =>
   (python-trans-file filename)
   (exit)]
  [else
   ; =>
   (python-trans-port (current-input-port))])
   
; Ashton Snelgrove
; u0662114
; snelgrov@eng.utah.edu
   
