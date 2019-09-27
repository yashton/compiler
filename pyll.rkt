#lang racket

(require "pytoc-common.rkt")

(define labels '())

(define (ll-transform-program input) 
	(match input
		[`(program ,exprs ...)
			(define body (map (lambda (expr) (ll-transform expr)) exprs))
			`(program ,@labels ,@body)]))
				
(define (ll-transform input) 
	(match input 
		[`(define ,_ ...) input]
		[`(define-env ,_ ...) input]
		[`(,(and op (or 'error 'make-cell 'get-cell)) ,aexp)
			; =>
			`(,op ,(ll-transform aexp))]
		[`(if ,test ,true ,false)
			; =>
			`(if ,(ll-transform test)
				,(ll-transform true)
				,(ll-transform false))]
		[`(,(and op (or 'set-then! 'set-cell!)) ,var ,val ,then) 
			; =>
			`(,op ,var 
				,(ll-transform val)
				,(ll-transform then))]
		[`(dict (,k ,v) ...)
		 `(dict
		 	,@(map 
		 		(lambda (k v)
		 			`(,(ll-transform k)
		 				,(ll-transform v)))
		 		k v))] 
		[(? basic-nonvar?) input]
		[(? primitive-operation?) input]
		[(? symbol?) input]
		[`(lambda (,vars ...) ,body)
			; =>
			(let ([label (gensym '$lambda)])
				(set! labels 
					(cons `(define-label ,label (lambda ,vars ,(ll-transform body))) labels))
				`(lambda-label ,label))]
		[`(,f ,aexp ...)
			;=>
			`(,(ll-transform f)
				,@(map (lambda (aexp) (ll-transform aexp)) aexp))]
		[else `(foo ,input)]))

(pretty-write (ll-transform-program (read)))
