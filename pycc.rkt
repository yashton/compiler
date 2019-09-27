#lang racket
(require "pytoc-common.rkt")
(define environments '())

(define (set-member-f? s) (lambda (i) (set-member? s i)))

(define (cc-transform-program input) 
	(match input
		[`(program (define ,global ,val) ... ,exprs ...)
			(define body
					(map
						(lambda (expr) 
							(cc-transform-cexp (apply set global) (set) (set) '$env_global expr))
						exprs))
			`(program
				,@environments 
				,@(map (lambda (var val) `(define ,var ,val)) global val)
				,@body)]))

(define (cc-transform-cexp global env scope envname input) 
	(match input 
		[`(,(and op (or 'error 'make-cell 'get-cell)) ,aexp)
			; =>
			`(,op ,(cc-transform-cexp global env scope envname aexp))]
		[`(if ,test ,true ,false)
			; =>
			`(if ,(cc-transform-cexp global env scope envname test)
				,(cc-transform-cexp global env scope envname true)
				,(cc-transform-cexp global env scope envname false))]
		[`(,(and op (or 'set-then! 'set-cell!)) ,var ,val ,then) 
			; =>
			`(,op ,var 
				,(cc-transform-cexp global env scope envname val)
				,(cc-transform-cexp global env scope envname then))]
		[`(dict (,k ,v) ...)
		 `(dict
		 	,@(map 
		 		(lambda (k v)
		 			`(,(cc-transform-cexp global env scope envname k)
		 				,(cc-transform-cexp global env scope envname v)))
		 		k v))] 
		[(? basic-nonvar?) input]
		[(? primitive-operation?) input]
		[(? symbol?)
			(cond [(set-member? scope input) input]
						[(set-member? global input) input]
						[(set-member? env input) `(env-ref ,envname $env ,input)])]
		[`(lambda (,vars ...) ,body)
			; =>
			(let ([new-scope (apply set vars)]
						[new-env (set-union env scope)]
						[new-envname (gensym '$env_t)])
				(let ([closed (set-subtract (cc-find-aexp body)
																		(set-union new-scope global))]
							[new-global (set-subtract global new-scope)])		
					(set! environments
						(cons
							`(define-env ,new-envname ,(for/list ([c closed]) c))
							environments))
					`(make-closure
						(lambda ($env ,@vars)
							,(cc-transform-cexp new-global new-env new-scope new-envname body))
							(make-env ,new-envname 
								,@(for/list ([c closed])
									`(,c 
										,(if (set-member? env c)
													`(env-ref ,envname $env ,c)
													c)))))))]
		[`((,(or 'λ 'lambda) . ,lamb) ,aexps ...)		
			`(app* 
				,(cc-transform-cexp  global env scope envname `(lambda ,@lamb))
				,@(map (lambda (aexp) 
								(cc-transform-cexp global env scope envname aexp)) aexps))]
		[`(,(and symb 
					(and (? symbol?) 
								(not (or (? basic-nonvar?) (? primitive-operation?)))))
					,aexps ...)
			`(app* ,(if (set-member? env symb) `(env-ref ,envname $env ,symb) symb)
				,@(map (lambda (aexp) (cc-transform-cexp global env scope envname aexp)) aexps))]
		[`(,f ,aexp ...)
			;=>
			`(,(cc-transform-cexp global env scope envname f)
				,@(map (lambda (aexp) (cc-transform-cexp global env scope envname aexp)) aexp))]))
				
(define (cc-find-aexp input) 
	(match input 
		[`(,(and op (or 'error 'make-cell 'get-cell)) ,aexp)
			; =>
			(cc-find-aexp aexp)]
		[`(if ,test ,true ,false)
			; =>
			(set-union
				(cc-find-aexp test)
				(cc-find-aexp true)
				(cc-find-aexp false))]
		[`(,(and op (or 'set-then! 'set-cell!)) ,var ,val ,then) 
			; =>
			(set-union 
				(cc-find-aexp val)
				(cc-find-aexp then))]
		[`(dict (,k ,v) ...)
		 (set-union*
		 	(map 
		 		(lambda (k v)
		 			(set-union 
		 				(cc-find-aexp k)
		 				(cc-find-aexp v)))
		 		k v))] 
		[(? basic-nonvar?) (set)]
		[(? symbol?) (set input)]
		[`(lambda (,vars ...) ,body) (set-subtract (cc-find-aexp body) (apply set vars))]
		[`(,f ,aexps ...) 
			(set-union
				(cc-find-aexp f)
				(set-union*
					(map (lambda (aexp) (cc-find-aexp aexp)) aexps)))]))

(pretty-write (cc-transform-program (read)))
