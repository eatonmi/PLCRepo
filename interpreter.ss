(load "9_primitives.ss")

(define convert-procedure
  (lambda (exp)
    (cond [(procedure? exp) '<interpreter-procedure>]
	  [(pair? exp) (cons (convert-procedure (car exp)) (convert-procedure (cdr exp)))]
	  [(vector? exp) (list->vector (convert-procedure (vector->list exp)))]
	  [else exp])))

(define interpret
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   [initial-environment '()]
	   [result (eval-tree (syntax-expand parse-tree) initial-environment)])
      (convert-procedure result))))

(define contains?
  (lambda (ls x)
    (cond
     [(null? ls) #f]
     [(equal? (car ls) x) #t]
     [else (contains? (cdr ls) x)])))

(define eval-define
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
				    (begin (set-car! vars (add-to-end (car vars) var))
					   (add-define (cdr expls) vars)))))))))))))

(define eval-tree
  (lambda (exp env)
    (cases expression exp
	   [var-exp (depth position) (apply-env env depth position)]
	   [free-exp (name) (apply-global name)]
	   [lit-exp (literal) literal]
	   [lambda-exp (var body)
		       (make-closure var body env)]
	   [begin-exp (exp-list)
		      (if (not (null? exp-list))
			  (if (null? (cdr exp-list))
			      (eval-tree (car exp-list) env)
			      (begin (eval-tree (car exp-list) env)
				     (eval-tree (begin-exp (cdr exp-list)) env))))]
	   [if-exp (test first second)
		   (if (eval-tree test env)
		       (eval-tree first env)
		       (eval-tree second env))]
	   [if-half-exp (test true)
			(if (eval-tree test env)
			    (eval-tree true env))]
	   [or-exp (vals)
		   (if (null? vals)
		       #f
		       (let ([first (eval-tree (car vals) env)])
			 (if first
			     first
			     (eval-tree (or-exp (cdr vals)) env))))]
	   [and-exp (vals)
		   (if (null? vals)
		       #t
		       (let ([first (eval-tree (car vals) env)])
			 (if first
			     (if (null? (cdr vals))
				 first
				 (eval-tree (and-exp (cdr vals)) env))
			     first)))]
	   [let-exp (bindings body) (eopl:error 'eval-tree "Somehow the let expression ~s was not caught by syntax-expand" exp)]
	   [letrec-exp (vars body)
		       (make-closure var body env)
		       
		       (eopl:error 'eval-tree "Somehow the letrec expression ~s was not caught by syntax-expand" exp)]
	   [named-let (funct vars body) (eopl:error 'eval-tree "Somehow the named let expression ~s was not caught by syntax-expand" exp)]
	   [cond-exp (conds) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-EXP!  It's super effective!  Interpreter faints...")]
	   [condition-exp (test action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCONDITION-EXP!  It's super effective!  Interpreter faints...")]
	   [cond-else (action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-ELSE!  It's super effective!  Interpreter faints...")]
	   [set-exp (var value)
		    (cond [(eqv? (car var) 'var-exp)
			   (let ([sym (apply-env-set env (cadr var) (caddr var))][val (eval-tree value env)])
			     (set-car! sym val))]
			  [(eqv? (car var) 'free-exp)
			   (set-cdr! (apply-global-set (cadr var)) (eval-tree value env))]
			  [else (eopl:error 'eval-tree "Invalid set! variable ~s" var)])]
	   [define-exp (var value)
	     (cond [(eqv? (car var) 'var-exp)
		    (let ([depth (cadr var)][position (caddr var)])
		      (if (and (eqv? depth 0)
			       (not (exist-pos? position (car env))))
			  (set-car! env (add-to-end (car env) (eval-tree value env)))
			  (eval-tree (set-exp var value) env)))]
		   [(eqv? (car var) 'free-exp)
		    (if (eqv? env '())
			(define-global (cadr var) (eval-tree value env))
			(eopl:error 'eval-tree "A define somehow escaped capture! ~s" exp))]
		   [else (eopl:error 'eval-tree "Invalid variable ~s in definition" var)])]
	   [case-exp (value clauses)
		     (cases clause (car clauses)
			    (case-clause (keys body)
					 (if (contains? keys (eval-tree value env))
					     (eval-tree body env)
					     (if (not (null? (cdr clauses)))
						 (eval-tree (case-exp value (cdr clauses)) env))))
			    (else-clause (body) (eval-tree body env)))]
	   [app-exp (operator operands)
		    (let ([procedure (eval-tree operator env)])
		      (if (equal? operator '(free-exp apply))
			  (apply-proc procedure operands env)
			  (let ([args (eval-list operands env)])
			    (apply-proc procedure args env))))]
	   [empty-exp () '()])))

(define eval-list
  (lambda (exp-list env)
    (if (null? exp-list)
	'()
	(if (list? exp-list)
	    (cons (eval-tree (car exp-list) env) (eval-list (cdr exp-list) env))
	    (eopl:error 'eval-list "error evaluating list structure ~s" exp-list)))))

(define make-closure
  (lambda (var body env)
    (closure var body env)))

(define-datatype procedure procedure?
  [closure
   (var var-list?)
   (body expression?)
   (env list?)]
  [primitive
    (id symbol?)])

(define apply-proc
  (lambda (proc args env)
    (if (procedure? proc)
	(cases procedure proc
	       [closure (var body env)
			(eval-tree body (extend-env var args env))]
	       [primitive (id)
			  (apply-primitive-proc id args env)])
	(proc args))))

(define apply-primitive-proc
  (lambda (id args env)
    (case id
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(add1) (+ (car args) 1)]
      [(sub1) (- (car args) 1)]
      [(zero?) (zero? (car args))]
      [(not) (not (car args))]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(<=) (apply <= args)]
      [(>=) (apply >= args)]
      [(cons) (cons (car args) (cadr args))]
      [(car) (caar args)]
      [(cdr) (cdar args)]
      [(list) (apply list args)]
      [(null?) (null? (car args))]
      [(eq?) (eq? (car args) (cadr args))]
      [(equal?) (equal? (car args) (cadr args))]
      [(atom?) (atom? (car args))]
      [(length) (length (car args))]
      [(list->vector) (list->vector (car args))]
      [(list?) (list? (car args))]
      [(pair?) (pair? (car args))]
      [(procedure?) (procedure? (car args))]
      [(vector->list) (vector->list (car args))]
      [(vector) (apply vector args)]
      [(make-vector) (make-vector (car args) (cadr args))]
      [(vector-ref) (vector-res (car args) (cadr args))]
      [(vector?) (vector? (car args))]
      [(number?) (number? (car args))]
      [(symbol?) (symbol? (car args))]
      [(set-car!) (set-car! (car args) (cadr args))]
      [(set-cdr!) (set-cdr! (car args) (cadr args))]
      [(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
      [(caaar) (car (car (car (car args))))]
      [(caadr) (car (car (cdr (car args))))]
      [(cadar) (car (cdr (car (car args))))]
      [(caddr) (car (cdr (cdr (car args))))]
      [(cdaar) (cdr (car (car (car args))))]
      [(cdadr) (cdr (car (cdr (car args))))]
      [(cddar) (cdr (cdr (car (car args))))]
      [(cdddr) (cdr (cdr (cdr (car args))))]
      [(caar) (car (car (car args)))]
      [(cadr) (car (cdr (car args)))]
      [(cdar) (cdr (car (car args)))]
      [(cddr) (cdr (cdr (car args)))]
      [(map) (primitive-map (car args) (cadr args) env)]
      [(apply) (primitive-apply (car args) (cadr args) env)]
      [(assq) (primitive-assq (car args) (cadr args))]
      [(assv) (primitive-assv (car args) (cadr args))]
      [(append) (primitive-append args)]
      [else (eopl:error 'apply-primitive-proc
              "Not a primitive procedure ~s" id)])))

(define syntax-expand-clauses
  (lambda (clauses)
    (if (null? clauses)
	'()
	(let ([new_clause (cases clause (car clauses)
				 [case-clause (keys body) (case-clause keys (syntax-expand body))]
				 [else-clause (body) (else-clause (syntax-expand body))])])
	 (cons new_clause (syntax-expand-clauses (cdr clauses))))))) 

(define syntax-expand
  (lambda (exp)
    (cases expression exp
	   [var-exp (depth position) exp]
	   [free-exp (name) exp]
	   [lit-exp (literal) exp]
	   [lambda-exp (var body) (lambda-exp var (syntax-expand body))]
	   [begin-exp (exp-list) (begin-exp (map syntax-expand exp-list))]
	   [if-exp (test first second)
		   (if-exp (syntax-expand test)
		       (syntax-expand first)
		       (syntax-expand second))]
	   [if-half-exp (test true)
			(if-half-exp (syntax-expand test)
			    (syntax-expand true))]
	   [or-exp (vals) (or-exp (map syntax-expand vals))]
	   [and-exp (vals) (and-exp (map syntax-expand vals))]
	   [let-exp (bindings body) (app-exp (lambda-exp (vars-list bindings) (syntax-expand body)) (map syntax-expand (exps-list bindings)))]
	   [letrec-exp (bindings body) (app-exp (lambda-exp (vars-list bindings) (syntax-expand body)) (map syntax-expand (exps-list bindings)))]
	   [named-let (funct vars body) 
		      (let ([newvars (append vars (list (list funct body)))])
			(syntax-expand (letrec-exp newvars (app-exp (var-exp 0 (length vars)) (placevars vars 0)))))]
	   [set-exp (var value) (set-exp var (syntax-expand value))]
	   [define-exp (var value) (define-exp var (syntax-expand value))]
	   [case-exp (value clauses) (case-exp (syntax-expand value) (syntax-expand-clauses clauses))]
	   [cond-exp (exp-pairs) (expand-conds exp-pairs)]
	   [condition-exp (test action) (eopl:error 'expand-syntax "Unhandled condition-exp")]
	   [cond-else (action) (eopl:error 'expand-syntax "Unhandled cond-else")]
	   [app-exp (operator operands)
		    (app-exp (syntax-expand operator)
			  (map syntax-expand operands))]
	   [empty-exp () '()])))

(define expand-conds(
		     lambda(conditions)
		      (cond [(eqv? (caar conditions) 'condition-exp) (if (not (null? (cdr conditions))) (if-exp (cadar conditions) (caddar conditions) (expand-conds (cdr conditions))) (if-half-exp (cadar conditions) (caddar conditions)))]
			    [(eqv? (caar conditions) 'cond-else) (cadar conditions)]
			    [else (eopl:error 'expand-conditions "Something went horribly wrong in parsing a cond")]      
			    )
		      )
  )
