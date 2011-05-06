(define var-list?
  (lambda (datum)
    (or (symbol? datum)
	(if (or (pair? datum)
		(list? datum))
	    (or (null? datum)
		(and (or (symbol? (car datum))
			 (and (list? (car datum))
			      (not (null? (car datum)))
			      (eqv? (caar datum) 'ref)
			      (not (null? (cdar datum)))
			      (symbol? (cadar datum))
			      (null? (cddar datum))))
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
  (or-exp
    (vals list))
  (and-exp
    (vals list?))
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
  (define-exp
    (id expression?)
    (value scheme-value?))
  (set-exp
    	(id expression?)
	(value scheme-value?))
  (letrec-exp
    	(vars list?)
	(body expression?))
  (named-let
  	(funct symbol?)
	(vars list?)
	(body expression?))
  (while-exp
    (test expression?)
    (body expression?))
   (ref-var
     (depth number?)
     (position number?)))

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

;(define let*->let
 ; (lambda (ls)
  ;  (if (null? (cadr ls))
;	(cddr ls)
;	(cons 'let (cons (cons (caadr ls) '()) (let*->let (cons 'let* (cons (cdadr ls) (cddr ls)))))))))

(define listlength(
        lambda(l n)
                (cond [(null? l) n]
                [else (listlength (cdr l) (+ 1 n))]
                )
        )
)

(define let*->let(
        lambda(l)
                (let*help '() (cadr l) (cddr l))
        )
)

(define let*help(
        lambda(l f p)
                (cond [(= 1 (listlength f 0)) (append l (list (append (list (car '(let)) (list (car f))) p)))]
                [else (let*help (append l (list (car '(let)) (list (car f)))) (cdr f) p)]
                )
        )
)


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
	    (if (or (eqv? var (car var-ls))
		    (and (list? (car var-ls))
			 (eqv? var (cadar var-ls))))
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

(define getlevel(
	lambda(level list)
	(if (= level 0) (car list) (getlevel (- level 1) (cadr list)))))

(define getpos(
	lambda(pos list)
		(if (= pos 0) (car list) (getpos (- pos 1) (cdr list)))))
(define isref(
	lambda(info vars)
		(let* ([level (getlevel (car info) vars)][var (getpos (cadr info) level)]) (if (and (list? var) (eq? 'ref (car var))) #t #f))))

(define add-define
  (lambda (expls vars)
    (if (list? expls)
	(if (not (null? expls))
	    (let ([first (car expls)])
	      (if (list? first)
		  (if (not (null? first))
		      (if (eqv? (car first) 'define)
			  (if (not (null? (cdr first)))
			      (let ([var (cadr first)])
				(if (symbol? var)
				    (begin (if (eqv? (car (parse-expression-vars var vars)) 'free-exp)
					       (set-car! vars (add-to-end (car vars) var)))
					   (add-define (cdr expls) vars)))))))))))))

(define parse-expression-vars
  (lambda (datum vars)
    (cond
      ((symbol? datum)
       (let ([info (find datum vars)])
	 (if info
	 ;    (if (isref info vars) (ref-var (car info) (cadr info))
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
		     (let ([new-vars (cons (cadr datum) (cons vars '()))])
		       (if (null? (cdddr datum))
			   (lambda-exp (cadr datum)
				       (parse-expression-vars (caddr datum) new-vars))
			   (lambda-exp (cadr datum)
				       (begin (add-define (cddr datum) new-vars)
					      (begin-exp (parse-exp-ls (cddr datum) new-vars))))))]
		    [else (eopl:error 'parse-expression
				      "Invalid variable bindings ~s" datum)])]
	     [(eqv? (car datum) 'begin)
	      (begin (add-define (cdr datum) vars)
		     (begin-exp (parse-exp-ls (cdr datum) vars)))]
	     [(eqv? (car datum) 'define)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression "Define expression without variable to bind ~s" datum)]
		    [(not (symbol? (cadr datum)))
		     (eopl:error 'parse-expression "Bad variable in define expression ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression "Define expression binding expression ~s" datum)]
		    [(not (null? (cdddr datum)))
		     (eopl:error 'parse-expression "Define expression with too many binding expressions ~s" datum)]
		    [else (let ([variable (parse-expression-vars (cadr datum) vars)])
			    (if (not (eqv? vars '()))
				(if (eqv? (car variable) 'free-exp)
				    (eopl:error 'parse-expression "Invalid context for definition ~s" datum)))
			    (define-exp variable (parse-expression-vars (caddr datum) vars)))])]
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
	     [(eqv? (car datum) 'or)
	      (or-exp (parse-exp-ls (cdr datum) vars))]
	     [(eqv? (car datum) 'and)
	      (and-exp (parse-exp-ls (cdr datum) vars))]
	     [(eqv? (car datum) 'let)
	      (cond [(null? (cdr datum))
		     (eopl:error 'parse-expression
				 "Let statement without bindings ~s" datum)]
		    [(null? (cddr datum))
		     (eopl:error 'parse-expression
				 "No body in let statement ~s" datum)]
		    ;;;Named Let Implementation
		    [(and (symbol? (cadr datum)) (list? (caddr datum)))
		     (if (null? (cdddr datum)) (eopl:error 'parse-expression "No body in named let expression ~s" datum)
			 (let ([new-vars (cons (vars-list (caddr datum)) (cons (cons (cons (cadr datum) '()) (cons vars '())) '()))])
			   (if (null? (cddddr datum))
			       (named-let (cadr datum) (parse-bindings (caddr datum) vars) (parse-expression-vars (cadddr datum) new-vars))
			       (named-let (cadr datum) (parse-bindings (caddr datum) vars) (begin (add-define (cdddr datum) new-vars) (begin-exp (parse-exp-ls (cdddr datum) new-vars)))))))]
		    ;;;End Named Let Implementation
												    [(not (list? (cadr datum)))
		     (eopl:error 'parse-expression
				 "Improper bindings in let statement ~s" datum)]
		    [else
		     (let ([new-vars (cons (vars-list (cadr datum)) (cons vars '()))])
		       (if (null? (cdddr datum))
			   (let-exp (parse-bindings (cadr datum) vars)
				    (parse-expression-vars (caddr datum) new-vars))
			   (let-exp (parse-bindings (cadr datum) vars)
				    (begin (add-define (cddr datum) new-vars)
					   (begin-exp (parse-exp-ls (cddr datum) new-vars))))))])]
	     [(eqv? (car datum) 'letrec) 
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
		     (let ([new-vars (cons (vars-list (cadr datum)) (cons vars '()))])
		       (if (null? (cdddr datum))
			   (letrec-exp (parse-bindings (cadr datum) new-vars)
				       (parse-expression-vars (caddr datum) new-vars))
			   (letrec-exp (parse-bindings (cadr datum) new-vars)
				       (begin (add-define (cddr datum) new-vars)
					      (begin-exp (parse-exp-ls (cddr datum) new-vars))))))])]
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
	     [(eqv? (car datum) 'while)
	      (cond [(null? (cdr datum)) (eopl:error 'parse-expression "While expression without test or body ~s" datum)]
		    [(null? (cddr datum)) (eopl:error 'parse-expression "While expression without body ~s" datum)]
		    [else (if (null? (cdddr datum))
			      (while-exp (parse-expression-vars (cadr datum) vars) (parse-expression-vars (caddr datum) vars))
			      (while-exp (parse-expression-vars (cadr datum) vars) (begin (add-define (cddr datum) vars) (begin-exp (parse-exp-ls (cddr datum) vars)))))])]
	     [(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
	     [(eqv? (car datum) 'set!) (set-exp (parse-expression-vars (cadr datum) vars) (parse-expression-vars (caddr datum) vars))]
	     [else (app-exp
		    (parse-expression-vars (car datum) vars)
		    (parse-exp-ls (cdr datum) vars))]))
      ((null? datum) (empty-exp))
      (else (eopl:error 'parse-expression
              "Invalid concrete syntax ~s" datum)))))

(define add-to-end
  (lambda (vars var)
    (if (null? vars)
	(cons var vars)
	(cons (car vars) (add-to-end (cdr vars) var)))))

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
	    (get-pos-set (- pos 1) (cdr var-ls))))))

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
