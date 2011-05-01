(define var-list?
  (lambda (datum)
    (or (symbol? datum)
	(if (or (pair? datum)
		(list? datum))
	    (or (null? datum)
		(and (symbol? (car datum))
		     (var-list? (cdr datum))))
	    #f))))

(define scheme-value?
  (lambda (v)
    #t))

(define-datatype expression expression?
  (empty-exp)
  (var-exp
    (depth number?)
    (position number?))
  (free-exp
    (name symbol?))
  (lambda-exp
    (vars var-list?)
    (body expression?))
  (begin-exp
    (exp-ls list?))
  (if-exp
    (test expression?)
    (first expression?)
    (second expression?))
  (if-half-exp
    (test expression?)
    (true expression?))
  (let-exp
    (bindings list?)
    (body expression?))
  (case-exp
    (value expression?)
    (clauses list-of-clauses?))
  (app-exp
    (rator expression?)
    (rand list?))
  (cond-exp
    (exp-pairs scheme-value?))
  (condition-exp
    (test scheme-value?)
    (action scheme-value?))
  (cond-else
    (action scheme-value?))
  (lit-exp
    (literal scheme-value?))
  (set-exp
    	(id expression?)
	(value scheme-value?)))

(define list-of-clauses?
  (lambda (exp)
    (if (null? exp)
	#t
	(if (not (list? exp))
	    #f
	    (and (clause? (car exp))
		 (list-of-clauses? (cdr exp)))))))

(define-datatype clause clause?
  (case-clause
    (keys list?)
    (body expression?))
  (else-clause
    (body expression?)))

(define parse-exp-ls
  (lambda (datum vars)
    (cond [(null? datum) '()]
	  [(not (list? datum))
	   (eopl:error 'parse-expression
              "Invalid expression list ~s" datum)]
	  [else (cons (parse-expression-vars (car datum) vars) (parse-exp-ls (cdr datum) vars))])))

(define parse-cond-exps(
	lambda(datum vars result)
		(cond [(null? datum) result]
		[(eqv? (caar datum) 'else) (append result (list (cond-else (parse-expression-vars (cadar datum) vars))))]
		[else (parse-cond-exps (cdr datum) vars (append result (list (condition-exp (parse-expression-vars (caar datum) vars) (parse-expression-vars (cadar datum) vars)))))]
		)
	)
 )
(define vars-list
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (caar bindings) (vars-list (cdr bindings))))))

(define exps-list
  (lambda (bindings)
    (if (null? bindings)
	'()
	(cons (cadar bindings) (exps-list (cdr bindings))))))

(define parse-bindings
  (lambda (datum vars)
    (cond [(null? datum) '()]
	  [(symbol? datum)
	   (eopl:error 'parse-expression
				 "Not a list of bindings ~s" datum)]
	  [(not (list? (car datum)))
	   (eopl:error 'parse-expression
				 "First element is not a binding pair ~s" (car datum))]
	  [(null? (cdar datum))
	   (eopl:error 'parse-expression
				 "No expression in binding ~s" (car datum))]
	  [(not (null? (cddar datum)))
	   (eopl:error 'parse-expression
				 "Too many expressions in binding ~s" (car datum))]
	  [(symbol? (caar datum))
	   (cons (list (caar datum)
		       (parse-expression-vars (cadar datum) vars))
		 (parse-bindings (cdr datum) vars))]
	  [else (eopl:error 'parse-expression
				 "Invalid variable in binding ~s" (car datum))])))

(define parse-clauses
  (lambda (datum vars)
    (cond [(null? datum) '()]
	  [(not (list? datum))
	   (eopl:error 'parse-expression "Not a list of clauses ~s" datum)]
	  [(not (list? (car datum)))
	   (eopl:error 'parse-expression "Not a clause ~s" datum)]
	  [(null? (car datum))
	   (eopl:error 'parse-expression "Empty clauses ~s" (car datum))]
	  [(not (list? (caar datum)))
	   (if (eqv? (caar datum) 'else)
	       (if (null? (cddar datum))
		   (cons (else-clause (parse-expression-vars (cadar datum) vars)) (parse-clauses (cdr datum) vars))
		   (cons (else-clause (begin-exp (parse-exp-ls (cadar datum) vars))) (parse-clauses (cdr datum) vars)))
	       (eopl:error 'parse-expression "Not a key list ~s" (caar datum)))]
	  [(null? (cdar datum))
	   (eopl:error 'parse-expression "Missing expression for key(s) ~s" (caar datum))]
	  [else (if (null? (cddar datum))
		    (cons (case-clause (caar datum) (parse-expression-vars (cadar datum) vars)) (parse-clauses (cdr datum) vars))
		    (cons (case-clause (caar datum) (begin-exp (parse-exp-ls (cadar datum) vars))) (parse-clauses (cdr datum) vars)))])))

(define let*->let
  (lambda (ls)
    (if (null? (cadr ls))
	(caddr ls)
	(list 'let (cons (caadr ls) '()) (let*->let (list 'let* (cdadr ls) (caddr ls)))))))

(define parse-expression
  (lambda (datum)
    (parse-expression-vars datum '())))

(define find-pos
  (lambda (var var-ls)
    (if (null? var-ls)
	#f
	(if (symbol? var-ls)
	    (if (eqv? var var-ls)
		0
		#f)
	    (if (eqv? var (car var-ls))
		0
		(let ([pos (find-pos var (cdr var-ls))])
		  (if pos
		      (+ pos 1)
		      #f)))))))

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

(define parse-expression-vars
  (lambda (datum vars)
    (cond
      ((symbol? datum)
       (let ([info (find datum vars)])
	 (if info
	     (var-exp (car info) (cadr info))
	     (free-exp datum))))
      ((or (number? datum)
	   (or (string? datum)
	       (or (vector? datum)
		   (or (eqv? datum '#t)
		       (eqv? datum '#f)))))
       (lit-exp datum))
      ((pair? datum)
       (cond [(eqv? (car datum) 'lambda)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression
				 "No variables or body in lambda expression ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression
				 "No body in lambda expression ~s" datum)]
		    [(var-list? (cadr datum))
		     (if (null? (cdddr datum))
			 (lambda-exp (cadr datum)
				     (parse-expression-vars (caddr datum) (cons (cadr datum) (cons vars '()))))
			 (lambda-exp (cadr datum)
				     (begin-exp (parse-exp-ls (cddr datum) (cons (cadr datum) (cons vars '()))))))]
		    [else (eopl:error 'parse-expression
				      "Invalid variable bindings ~s" datum)])]
	     [(eqv? (car datum) 'begin)
	      (begin-exp (parse-exp-ls (cdr datum) vars))]
	      [(eqv? (car datum) 'cond)
	      (cond-exp (parse-cond-exps (cdr datum) vars '()))]
	     [(eqv? (car datum) 'if)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression
				 "If statement without condition ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression
				 "If statement without expression for true ~s" datum)]
		    [(null? (cdddr datum))
		     (if-half-exp (parse-expression-vars (cadr datum) vars)
			     (parse-expression-vars (caddr datum) vars))]
		    [(null? (cddddr datum))
		     (if-exp (parse-expression-vars (cadr datum) vars)
			     (parse-expression-vars (caddr datum) vars)
			     (parse-expression-vars (cadddr datum) vars))]
		    [else (eopl:error 'parse-expression
				 "If statement with extra expressions ~s" datum)])]
	     [(eqv? (car datum) 'let)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression
				 "Let statement without bindings ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression
				 "No body in let statement ~s" datum)]
		    [(not (list? (cadr datum)))
		     (eopl:error 'parse-expression
				 "Improper bindings in let statement ~s" datum)]
		    [else
		     (if (null? (cdddr datum))
			 (let-exp (parse-bindings (cadr datum) vars)
			      (parse-expression-vars (caddr datum) (cons (vars-list (cadr datum)) (cons vars '()))))
			 (let-exp (parse-bindings (cadr datum) vars)
				  (begin-exp (parse-exp-ls (cddr datum) (cons (vars-list (cadr datum)) (cons vars '()))))))])]
	     [(eqv? (car datum) 'let*)
	      (parse-expression-vars (let*->let datum) vars)]
	     [(eqv? (car datum) 'case)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression
				 "Case expression without expression ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression
				 "Case expression without clauses ~s" datum)]
		    				 
		    [else (case-exp (parse-expression-vars (cadr datum) vars) (parse-clauses (cddr datum) vars))])]
	     [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
	     [(eqv? (car datum) 'set!) (set-exp (parse-expression-vars (cadr datum) vars) (parse-expression-vars (caddr datum) vars))]
	     [else (app-exp
		    (parse-expression-vars (car datum) vars)
		    (parse-exp-ls (cdr datum) vars))]))
      ((null? datum) (empty-exp))
      (else (eopl:error 'parse-expression
              "Invalid concrete syntax ~s" datum)))))

(define unparse-exp-ls
  (lambda (varls vars)
    (cond [(null? varls) '()]
	  [else (cons (unparse-expression-vars (car varls) vars) (unparse-exp-ls (cdr varls) vars))])))

(define unparse-expression
  (lambda (exp)
    (unparse-expression-vars exp '())))

(define get-pos
  (lambda (pos var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? pos 0)
	    (car var-ls)
	    (get-pos (- pos 1) (cdr var-ls))))))

(define get-pos-set
  (lambda (pos var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? pos 0)
	    var-ls
	    (get-pos (- pos 1) (cdr var-ls))))))

(define get
  (lambda (info var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? (car info) 0)
	    (get-pos (cadr info) (car var-ls))
	    (get (cons (- (car info) 1) (cdr info)) (cadr var-ls))))))

(define unparse-expression-vars
  (lambda (exp vars)
    (cases expression exp
      (var-exp (depth position)
	       (get (cons depth (cons position '())) vars))
      (free-exp (name) name)
      (lambda-exp (var body) 
        (cons 'lambda (cons var (unparse-exp-vars body (cons var (cons vars '()))))))
      (if-exp (test first second)
	      (list 'if (unparse-expression-vars test vars)
		    (unparse-expression-vars first vars)
		    (unparse-expression-vars second vars)))
      (if-half-exp (test true)
		   (list 'if (unparse-expression-vars test vars)
			 (unparse-expression-vars true vars)))
      (app-exp (rator rand)
        (cons (unparse-expression-vars rator vars)
              (unparse-exp-ls rand vars)))
      (lit-exp (literal) literal)
      (empty-exp () '()))))
