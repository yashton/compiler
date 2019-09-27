#lang racket
(require "pytoc-common.rkt")

;(define (mve-transform-program input) 
;	(match input
;		[`(program (define ,global ,val) ... ,exprs ...)
;			`(program 
;				,@(map (lambda (var val) `(define ,var ,val)) global val)
;				,@(map (lambda (expr) (mve-transform-cexp '() global expr)) exprs))]))

;(define (mve-transform-cexp env global input) 
;	(match input 
;		[`(error ,aexp)
;			; =>
;			`(error ,(mve-transform-aexp env global aexp))]
;		[`(if ,test ,true ,false)
;			; =>
;			`(if ,(mve-transform-aexp env global test)
;				,(mve-transform-cexp env global true)
;				,(mve-transform-cexp env global false))]
;		[`(set-then! ,var ,val ,then) 
;			; =>
;			(let ([new-global (cons var global)])
;			`(set-then! ,var 
;				,(mve-transform-aexp env new-global val)
;				,(mve-transform-cexp env new-global then)))]
;		[`(,f ,aexp ...)
;			;=>
;			`(,(mve-transform-aexp env global f)
;				,@(map (lambda (aexp) (mve-transform-aexp env global aexp)) aexp))]))
;			
;(define (mve-transform-aexp env global input) 
;	(match input
;		[`(dict (,k ,v) ...)
;		 `(dict
;		 	,(map 
;		 		(lambda (k v)
;		 			`(,(mve-transform-aexp env global k)
;		 				,(mve-transform-aexp env global v)))
;		 		k v))] 
;		[(? basic-nonvar?) input]
;		[(? symbol?) 
;			(if (or (member input global)
;							(member input env)) 
;							input 
;							`(get-cell ,input))]
;		[`(lambda (,vars ...) ,body)
;			; =>
;			(let ([new-env (append env vars)])
;				`(lambda (,@vars) ,(mve-transform-cexp new-env global body)))]
;		[`(,f ,aexp ...) 
;			`(,(mve-transform-aexp env global f)
;				,@(map (lambda (aexp) (mve-transform-aexp env global aexp)) aexp))]))

(define (mve-transform-program free input) 
	(match input
		[`(program (define ,global ,val) ... ,exprs ...)
			`(program 
				,@(map (lambda (var val) `(define ,var ,val)) global val)
				,@(map (lambda (expr) (mve-transform-cexp free expr)) exprs))]))

(define (mve-transform-cexp free input) 
	(match input 
		[`(error ,aexp)
			; =>
			`(error ,(mve-transform-aexp free aexp))]
		[`(if ,test ,true ,false)
			; =>
			`(if ,(mve-transform-aexp free test)
				,(mve-transform-cexp free true)
				,(mve-transform-cexp free false))]
		[`(set-then! ,var ,val ,then) 
			; =>
			(let ([op (if (set-member? free var) 'set-cell! 'set-then!)])
				`(,op ,var 
					,(mve-transform-aexp free val)
					,(mve-transform-cexp free then)))]
		[`(,f ,aexp ...)
			;=>
			`(,(mve-transform-aexp free f)
				,@(map (lambda (aexp) (mve-transform-aexp free aexp)) aexp))]))
			
(define (mve-transform-aexp free input) 
	(match input
		[`(dict (,k ,v) ...)
		 `(dict
		 	,@(map 
		 		(lambda (k v)
		 			`(,(mve-transform-aexp free k)
		 				,(mve-transform-aexp free v)))
		 		k v))] 
		[(? basic-nonvar?) input]
		[(? symbol?) 
			(if (set-member? free input)
					`(get-cell ,input)
					input)]
		[`(lambda (,vars ...) ,body)
			; =>
			`(lambda (,@vars) 
					,(mve-lambda free vars (mve-transform-cexp free body)))]
		[`(,f ,aexp ...) 
			`(,(mve-transform-aexp free f)
				,@(map (lambda (aexp) (mve-transform-aexp free aexp)) aexp))]))

(define (mve-lambda free vars body)
	(if (empty? vars)
		body
		(let ([var (car vars)]
					[rest (mve-lambda free (cdr vars) body)])
			(if (set-member? free var)
					`(set-then! ,var (make-cell ,var) ,rest)
					rest))))
	

; Finds all non-global mutable variables
(define (mve-find input) 
	(match input 
		[`(program (define ,global ,val) ... ,exprs ...)
			(set-subtract (set-union* (map mve-find exprs)) (apply set global))]
		[`(error ,aexp) (mve-find aexp)]
		[`(if ,test ,true ,false)
			; =>
			(set-union 
				(mve-find test)
				(mve-find true)
				(mve-find false))]
		[`(set-then! ,var ,val ,then) 
			; =>
			(set-union (set var) 
				(mve-find val)
				(mve-find then))]
		[`(dict (,k ,v) ...)
		 	; =>
		 	(set-union* 
		 		(map 
			 		(lambda (k v)
			 			(set-union
			 				(mve-find k)
			 				(mve-find v)))
			 		k v))] 
		[(? basic-nonvar?) (set)]
		[(? symbol?)  (set)]
		[`(lambda (,vars ...) ,body)
			; =>
			(mve-find body)]
		[`(,f ,aexp ...) (set-union* (map mve-find input))]))
		
(let ([in (read)])
	(pretty-write (mve-transform-program (mve-find in) in)))
