;Lexical address
(define find-pos
  (lambda (var var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? var (car var-ls))
	    0
	    (let ([pos (find-pos var (cdr var-ls))])
	      (if pos
		  (+ pos 1)
		  #f))))))

(define find
  (lambda (var var-ls)
    (if (null? var-ls)
	#f
	(let ([pos (find-pos var (car var-ls))])
	  (if pos
	      (cons 0 (cons pos '()))
	      (let ([info (find var (cadr var-ls))])
		(if info
		    (cons (+ (car info) 1) (cdr info))
		    #f)))))))

(define convert-let
  (lambda (exp vars new-vars)
    (if (null? (car exp))
	(cons '() (convert-vars (cdr exp) (cons new-vars (cons vars '()))))
	(let ([converted (convert-let (cons (cdar exp) (cdr exp)) vars new-vars)])
	  (cons (cons (cons (caaar exp) (convert-vars (cdaar exp) vars)) (car converted)) (cdr converted))))))

(define convert-let*
  (lambda (exp vars)
    (if (null? (car exp))
	(cons '() (convert-vars (cdr exp) vars))
	(let ([converted (convert-let* (cons (cdar exp) (cdr exp)) (cons (cons (caaar exp) '()) (cons vars '())))])
	  (cons (cons (cons (caaar exp) (convert-vars (cdaar exp) vars)) (car converted)) (cdr converted))))))

(define vars-list
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (caar bindings) (vars-list (cdr bindings))))))

(define convert-vars
  (lambda (exp vars)
    (cond [(null? exp) exp]
	  [(number? exp) exp]
	  [(symbol? exp)
	   (let ([info (find exp vars)])
	     (if (eqv? info #t)
		 (list ': 'free exp)
		 (cons ': info)))]
	  [(eqv? (car exp) 'lambda)
	   (list (car exp) (cadr exp) (convert-vars (caddr exp) (cons (cadr exp) (cons vars '()))))]
	  [(eqv? (car exp) 'if)
	   (list (car exp) (convert-vars (cadr exp) vars) (convert-vars (caddr exp) vars) (convert-vars (cadddr exp) vars))]
	  [(eqv? (car exp) 'let)
	   (cons 'let (convert-let (cdr exp) vars (vars-list (cadr exp))))]
	  [(eqv? (car exp) 'let*)
	   (cons 'let* (convert-let* (cdr exp) vars))]
	  [(eqv? (car exp) 'set!)
	   (list (car exp) (convert-vars (cadr exp) vars) (convert-vars (caddr exp) vars))]
	  [else (cons (convert-vars (car exp) vars)
		      (convert-vars (cdr exp) vars))])))

(define lexical-address
  (lambda (exp)
    (convert-vars exp '())))

;Un-lexical address
(define get-pos
  (lambda (pos var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? pos 0)
	    (car var-ls)
	    (get-pos (- pos 1) (cdr var-ls))))))

(define get
  (lambda (info var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? (car info) 0)
	    (get-pos (cadr info) (car var-ls))
	    (get (cons (- (car info) 1) (cdr info)) (cadr var-ls))))))

(define convert-let-back
  (lambda (exp vars new-vars)
    (if (null? (car exp))
	(cons '() (convert-back (cdr exp) (cons new-vars (cons vars '()))))
	(let ([converted (convert-let-back (cons (cdar exp) (cdr exp)) vars new-vars)])
	  (cons (cons (cons (caaar exp) (convert-back (cdaar exp) vars)) (car converted)) (cdr converted))))))

(define convert-let*-back
  (lambda (exp vars)
    (if (null? (car exp))
	(cons '() (convert-back (cdr exp) vars))
	(let ([converted (convert-let*-back (cons (cdar exp) (cdr exp)) (cons (cons (caaar exp) '()) (cons vars '())))])
	  (cons (cons (cons (caaar exp) (convert-back (cdaar exp) vars)) (car converted)) (cdr converted))))))

(define convert-back
  (lambda (exp vars)
    (cond [(null? exp) exp]
	  [(atom? exp) exp]
	  [(eqv? (car exp) ':)
	   (if (eqv? (cadr exp) 'free)
		 (caddr exp)
		 (get (cdr exp) vars))]
	  [(eqv? (car exp) 'lambda)
	   (list (car exp) (cadr exp) (convert-back (caddr exp) (cons (cadr exp) (cons vars '()))))]
	  [(eqv? (car exp) 'if)
	   (list (car exp) (convert-back (cadr exp) vars) (convert-back (caddr exp) vars) (convert-back (cadddr exp) vars))]
	  [(eqv? (car exp) 'let)
	   (cons 'let (convert-let-back (cdr exp) vars (vars-list (cadr exp))))]
	  [(eqv? (car exp) 'let*)
	   (cons 'let* (convert-let*-back (cdr exp) vars))]
	  [(eqv? (car exp) 'set!)
	   (list (car exp) (convert-back (cadr exp) vars) (convert-back (caddr exp) vars))]
	  [else (cons (convert-back (car exp) vars)
		      (convert-back (cdr exp) vars))])))

(define un-lexical-address
  (lambda (exp)
    (convert-back exp '())))
