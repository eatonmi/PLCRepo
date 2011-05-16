(load "9_primitives.ss")

;For temporary fixes
(define make-cps
  (lambda (func)
    (lambda (x k)
      (k (func x)))))

(define ret
  (lambda (x)
    x))

;Temporary measure
;(define list?-cps
 ; (make-cps list?))

;(define list?
 ; (lambda (datum)
  ;  (if (null? datum)
;	#t
;	(if (pair? datum)
;	    (list (cdr datum))
;	    #f))))

(define list?-cps
  (lambda (datum k)
    (if (null? datum)
	(k #t)
	(if (pair? datum)
	    (list?-cps (cdr datum) k)
	    (k #f)))))

;(define convert-procedure
 ; (lambda (exp)
  ;  (cond [(procedure? exp) '<interpreter-procedure>]
;	  [(pair? exp) (cons (convert-procedure (car exp)) (convert-procedure (cdr exp)))]
;	  [(vector? exp) (list->vector (convert-procedure (vector->list exp)))]
;	  [else exp])))

(define vector->list-cps
  (make-cps vector->list))

(define list->vector-cps
  (make-cps list->vector))

(define convert-procedure-cps
  (lambda (exp k)
    (if (procedure? exp)
	(k '<interpreter-procedure>)
	(if (pair? exp)
	    (convert-procedure-cps (car exp) (lambda (v1) (convert-procedure-cps (cdr exp) (lambda (v2) (k (cons v1 v2))))))
	    (if (vector? exp)
		(vector->list-cps exp (lambda (v1) (convert-procedure-cps v1 (lambda (v2) (list->vector-cps v2 k)))))
		;(vector->list-cps exp (lambda (v1) (convert-procedure-cps v1 (lambda (v2) (list->vector-cps (lambda (v3) (k v3)))))))
		(k exp))))))

;(define interpret
 ; (lambda (exp)
  ;  (let* ([parse-tree (parse-expression exp)]
;	   [initial-environment '()]
;	   [result (eval-tree (syntax-expand parse-tree) initial-environment)])
 ;     (convert-procedure result))))

(define interpret-cps
  (lambda (exp k)
    (let ([parse-tree (parse-expression exp)][initial-environment '()])
      (eval-tree-cps (syntax-expand parse-tree) initial-environment (lambda (v) (convert-procedure-cps v k))))))

;(define contains?
 ; (lambda (ls x)
  ;  (cond
   ;  [(null? ls) #f]
    ; [(equal? (car ls) x) #t]
     ;[else (contains? (cdr ls) x)])))

(define contains?-cps
  (lambda (ls x k)
    (cond
     [(null? ls) (k #f)]
     [(equal? (car ls) x) (k #t)]
     [else (contains? (cdr ls) x k)])))

;(define eval-define
 ; (lambda (expls vars)
  ;  (if (list? expls)
;	(if (not (null? expls))
;	    (let ([first (car expls)])
;	      (if (list? first)
;		  (if (not (null? first))
;		      (if (eqv? (car first) 'define)
;			  (if (not (null? (cdr first)))
;			      (let ([var (cadr first)])
;				(if (symbol? var)
;				    (begin (set-car! vars (add-to-end (car vars) var))
;					   (eval-define (cdr expls) vars)))))))))))))

(define eval-define-cps
  (lambda (expls vars k)
    (if (pair? expls)
	(if (not (null? expls))
	    (let ([first (car expls)])
	      (if (pair? first)
		  (if (not (null? first))
		      (if (eqv? (car first) 'define)
			  (if (not (null? (cdr first)))
			      (let ([var (cadr first)])
				(if (symbol? var)
				    (add-to-end-cps (car vars) var (lambda (v1) (eval-define-cps (cdr expls) vars (lambda (v2) (k (begin (set-car! vars v1) v2)))))))))))))))))

;(define eval-tree
 ; (lambda (exp env)
  ;  (cases expression exp
;	   [var-exp (depth position) (apply-env env depth position)]
;	   [ref-var (depth position) (apply-env env depth position)]
;	   [free-exp (name) (apply-global name)]
;	   [lit-exp (literal) literal]
;	   [lambda-exp (var body)
;		       (make-closure var body env)]
;	   [begin-exp (exp-list)
;		      (if (not (null? exp-list))
;			  (if (null? (cdr exp-list))
;			      (eval-tree (car exp-list) env)
;			      (begin (eval-tree (car exp-list) env)
;				     (eval-tree (begin-exp (cdr exp-list)) env))))]
;	   [if-exp (test first second)
;		   (if (eval-tree test env)
;		       (eval-tree first env)
;		       (eval-tree second env))]
;	   [if-half-exp (test true)
;			(if (eval-tree test env)
;			    (eval-tree true env))]
;	   [or-exp (vals)
;		   (if (null? vals)
;		       #f
;		       (let ([first (eval-tree (car vals) env)])
;			 (if first
;			     first
;			     (eval-tree (or-exp (cdr vals)) env))))]
;	   [and-exp (vals)
;		   (if (null? vals)
;		       #t
;		       (let ([first (eval-tree (car vals) env)])
;			 (if first
;			     (if (null? (cdr vals))
;				 first
;				 (eval-tree (and-exp (cdr vals)) env))
;			     first)))]
;	   [let-exp (bindings body) (eopl:error 'eval-tree "Somehow the let expression ~s was not caught by syntax-expand" exp)]
;	   [letrec-exp (vars body)
;		       (let ([new-env (cons '() env)])
;			 (let ([var (vars-list vars)][args (eval-list (exps-list vars) new-env)])
;			   (letrec ([add (lambda (ls)
;					   (if (not (null? ls))
;					       (begin (set-car! new-env (add-to-end (car new-env) (car ls)))
;						      (add (cdr ls)))))])
;			     (add args))
;			   (eval-tree body new-env)))]
;	   [named-let (funct vars body) (eopl:error 'eval-tree "Somehow the named let expression ~s was not caught by syntax-expand" exp)]
;	   [cond-exp (conds) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-EXP!  It's super effective!  Interpreter faints... ~s" exp)]
;	   [condition-exp (test action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCONDITION-EXP!  It's super effective!  Interpreter faints... ~s" exp)]
;	   [cond-else (action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-ELSE!  It's super effective!  Interpreter faints...~s" exp)]
;	   [while-exp (test body)
;		      (let loop ([test-exp test])
;			(if (eval-tree test-exp env)
;			    (begin (eval-tree body env)
;				   (loop test-exp))))]
;		      ;(if (eval-tree test env)
;			;  (begin (eval-tree body env) (eval-tree exp env)))]
;	   [set-exp (var value)
;		    (cond [(eqv? (car var) 'var-exp)
;			   (let ([sym (apply-env-set env (cadr var) (caddr var))][val (eval-tree value env)])
;			     (set-car! sym val))]
;			  [(eqv? (car var) 'free-exp)
;			   (set-car! (apply-global-set (cadr var)) (eval-tree value env))]
;			  [else (eopl:error 'eval-tree "Invalid set! variable ~s" var)])]
;	   [define-exp (var value)
;	     (cond [(eqv? (car var) 'var-exp)
;		    (let ([depth (cadr var)][position (caddr var)])
;		      (if (and (eqv? depth 0)
;			       (not (exist-pos? position (car env))))
;			  (set-car! env (add-to-end (car env) (eval-tree value env)))
;			  (eval-tree (set-exp var value) env)))]
;		   [(eqv? (car var) 'free-exp)
;		    (if (eqv? env '())
;			(define-global (cadr var) (eval-tree value env))
;			(eopl:error 'eval-tree "A define somehow escaped capture! ~s" exp))]
;		   [else (eopl:error 'eval-tree "Invalid variable ~s in definition" var)])]
;	   [case-exp (value clauses)
;		     (cases clause (car clauses)
;			    (case-clause (keys body)
;					 (if (contains? keys (eval-tree value env))
;					     (eval-tree body env)
;					     (if (not (null? (cdr clauses)))
;						 (eval-tree (case-exp value (cdr clauses)) env))))
;			    (else-clause (body) (eval-tree body env)))]
;	   [app-exp (operator operands)
;		    (let ([procedure (eval-tree operator env)])
;		      (cond [(equal? operator '(free-exp apply))
;			  (apply-proc procedure operands env)]
;			  [else (let ([args (eval-list operands env)])
;				  (if (eqv? (car procedure) 'closure)
;				      ;Fix this later
;				      (apply-proc procedure args operands env)
;				      (apply-proc procedure args operands env)))]))]
;	   [empty-exp () '()])))

(define eval-tree-cps
  (lambda (exp env k)
    (cases expression exp
	   [var-exp (depth position) (apply-envCPS env depth position k)]
	   [ref-var (depth position) (apply-envCPS env depth position k)]
	   [free-exp (name) (apply-global name k)]
	   [lit-exp (literal) (k literal)]
	   [lambda-exp (var body)
		       (make-closure-cps var body env k)]
	   [begin-exp (exp-list)
		      (if (not (null? exp-list))
			  (if (null? (cdr exp-list))
			      (eval-tree-cps (car exp-list) env k)
			      (eval-tree-cps (car exp-list) env (lambda (v1) (begin v1 (eval-tree-cps (begin-exp (cdr exp-list)) env k))))))]
	   [if-exp (test first second)
		   (eval-tree-cps test env (lambda (v) (if v
		       (eval-tree-cps first env k)
		       (eval-tree-cps second env k))))]
	   [if-half-exp (test true)
			(eval-tree-cps test env (lambda (v)
					      (if v
						  (eval-tree-cps true env k))))]
	   [or-exp (vals)
		   (if (null? vals)
		       (k #f)
		       (eval-tree-cps (car vals) env (lambda (v)
						   (if v
						       v
						       (eval-tree-cps (or-exp (cdr vals)) env k)))))]
	   [and-exp (vals)
		   (if (null? vals)
		       (k #t)
		       (eval-tree-cps (car vals) env (lambda (v)
						   (if v
						       (if (null? (cdr vals))
							   v
							   (eval-tree-cps (and-exp (cdr vals)) env k))
						       v))))]
	   [let-exp (bindings body) (eopl:error 'eval-tree "Somehow the let expression ~s was not caught by syntax-expand" exp)]
	   [letrec-exp (vars body)
		       (let ([new-env (cons '() env)])
			 (exps-list-cps vars (lambda (v1)
					       (eval-list-cps v1 (lambda (v2) (k (letrec ([add (lambda (ls)
											      (if (not (null? ls))
												  (begin (set-car! new-env (add-to-end (car new-env) (car ls)))
													 (add (cdr ls)))))])
										(add args))									   (eval-tree body new-env)))))))]
	   [named-let (funct vars body) (eopl:error 'eval-tree "Somehow the named let expression ~s was not caught by syntax-expand" exp)]
	   [cond-exp (conds) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-EXP!  It's super effective!  Interpreter faints... ~s" exp)]
	   [condition-exp (test action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCONDITION-EXP!  It's super effective!  Interpreter faints... ~s" exp)]
	   [cond-else (action) (eopl:error 'eval-tree "Parse-exp uses IGNOREDCOND-ELSE!  It's super effective!  Interpreter faints...~s" exp)]
	   [while-exp (test body)
		      (eval-tree-cps test env (lambda (v1)
						(if v1
						    (eval-tree-cps body env (lambda (v2) (begin v2 (eval-tree-cps exp env k)))))))]
	   [set-exp (var value)
		    (cond [(eqv? (car var) 'var-exp)
			   (apply-env-setCPS env (cadr var) (caddr var) (lambda (v)
									  (k (set-car! v val))))]
			  [(eqv? (car var) 'free-exp)
			   (eval-tree-cps value env (lambda (v1) (apply-global-set (cadr var) (lambda (v2) (k (set-car! v2 v1))))))]
			  [else (eopl:error 'eval-tree "Invalid set! variable ~s" var)])]
	   [define-exp (var value)
	     (cond [(eqv? (car var) 'var-exp)
		    (let ([depth (cadr var)][position (caddr var)])
		      (exist-pos?cps position (car env) (lambda (v1)
							  (if (and (eqv? depth 0)
								   (not v1))
							      (eval-tree-cps value env (lambda (v2) (add-to-end-cps (car env) v2 (lambda (v3) (k (set-car! env v3))))))
							      (eval-tree-cps (set-exp var value) env k)))))]
		   [(eqv? (car var) 'free-exp)
		    (if (eqv? env '())
			(eval-tree-cps value env (lambda (v) (define-global (cadr var) v k)))
			(eopl:error 'eval-tree "A define somehow escaped capture! ~s" exp))]
		   [else (eopl:error 'eval-tree "Invalid variable ~s in definition" var)])]
	   [case-exp (value clauses)
		     (cases clause (car clauses)
			    (case-clause (keys body)
					 (eval-tree-cps value env (lambda (v1)
								(contains?-cps keys v1 (lambda (v2) (if v2
													(eval-tree-cps body env k)
													(if (not (null? (cdr clauses)))
													    (eval-tree (case-exp value (cdr clauses)) env) k))))))))
		     (else-clause (body) (eval-tree-cps body env k))]
;	   [app-exp (operator operands)
;		    (apply-proc (eval-tree operator env) (eval-list operands env) operands env)]
	   [app-exp (operator operands)
		    (eval-tree-cps operator env (lambda (v1) (eval-list-cps operands env (lambda (v2) (apply-proc-cps v1 v2 operands env k)))))]
	   [empty-exp () (k '())])))

;(define eval-list
 ; (lambda (exp-list env)
  ;  (if (null? exp-list)
;	'()
;	(if (list? exp-list)
;	    (cons (eval-tree (car exp-list) env) (eval-list (cdr exp-list) env))
;	    (eopl:error 'eval-list "error evaluating list structure ~s" exp-list)))))

(define eval-list-cps
  (lambda (exp-list env k)
    (if (null? exp-list)
	(k exp-list)
	(list?-cps exp-list (lambda (v1)
			      (if v1
				  (eval-tree-cps (car exp-list) env (lambda (v2) (eval-list-cps (cdr exp-list) env (lambda (v3) (k (cons v2 v3))))))
				  (eopl:error 'eval-list "error evaluating list structure ~s" exp-list)))))))
;	(list?-cps exp-list
;		   (lambda (v1) (if v1
;				    (eval-tree-cps (car exp-list) env (lambda (v1) (eval-list-cps (cdr exp-list) env (lambda (v2) (k v1 v2)))))
;				    (eopl:error 'eval-list "error evaluating list structure ~s" exp-list)))))))

(define make-closure-cps
  (lambda (var body env k)
    (k (closure var body env))))

(define-datatype procedure procedure?
  [closure
   (var var-list?)
   (body expression?)
   (env list?)]
  [primitive
    (id symbol?)])

;(define apply-proc
 ; (lambda (proc args operands env)
  ;  (if (procedure? proc)
;	(cases procedure proc
;	       [closure (var body env2)
;			(eval-tree body (extend-env var args operands env2 env))];
;	       [primitive (id)
;			  (apply-primitive-proc id args env)])
;	(proc args))))

(define apply-proc-cps
  (lambda (proc args operands env k)
    (cases procedure proc
	   [closure (var body env2)
		    (extend-envCPS var args operands env2 env (lambda (v) (eval-tree-cps body v k)))]
	   [primitive (id)
		      (apply-primitive-proc-cps id args env k)])))

;(define apply-primitive-proc
;  (lambda (id args env)
;    (case id
;      [(+) (apply + args)]
;      [(-) (apply - args)]
;      [(*) (apply * args)]
;      [(/) (apply / args)]
;      [(add1) (+ (car args) 1)]
;      [(sub1) (- (car args) 1)]
;      [(zero?) (zero? (car args))]
;      [(not) (not (car args))]
;      [(=) (apply = args)]
;      [(<) (apply < args)]
;      [(>) (apply > args)]
;      [(<=) (apply <= args)]
;      [(>=) (apply >= args)]
;      [(cons) (cons (car args) (cadr args))]
;      [(car) (caar args)]
;      [(cdr) (cdar args)]
;      [(list) (apply list args)]
;      [(null?) (null? (car args))]
;      [(eq?) (eq? (car args) (cadr args))]
;      [(equal?) (equal? (car args) (cadr args))]
;      [(atom?) (atom? (car args))]
;      [(length) (length (car args))]
;      [(list->vector) (list->vector (car args))]
;      [(list?) (list? (car args))]
;      [(pair?) (pair? (car args))]
;      [(procedure?) (procedure? (car args))]
;      [(vector->list) (vector->list (car args))]
;      [(vector) (apply vector args)]
;      [(make-vector) (make-vector (car args) (cadr args))]
;      [(vector-ref) (vector-res (car args) (cadr args))]
;      [(vector?) (vector? (car args))]
;      [(number?) (number? (car args))]
;      [(symbol?) (symbol? (car args))]
;      [(set-car!) (set-car! (car args) (cadr args))]
;      [(set-cdr!) (set-cdr! (car args) (cadr args))]
;      [(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
;      [(caaar) (car (car (car (car args))))]
;      [(caadr) (car (car (cdr (car args))))]
;      [(cadar) (car (cdr (car (car args))))]
;      [(caddr) (car (cdr (cdr (car args))))]
;      [(cdaar) (cdr (car (car (car args))))]
;      [(cdadr) (cdr (car (cdr (car args))))]
;      [(cddar) (cdr (cdr (car (car args))))]
;      [(cdddr) (cdr (cdr (cdr (car args))))]
;      [(caar) (car (car (car args)))]
;      [(cadr) (car (cdr (car args)))]
;      [(cdar) (cdr (car (car args)))]
;      [(cddr) (cdr (cdr (car args)))]
;      [(map) (primitive-map (car args) (cadr args) env)]
;      [(apply) (primitive-apply (car args) (cadr args) env)]
;      [(assq) (primitive-assq (car args) (cadr args))]
;      [(assv) (primitive-assv (car args) (cadr args))]
;      [(append) (primitive-append args)]
;      [else (eopl:error 'apply-primitive-proc
;			"Not a primitive procedure ~s" id)])))

;Temporary fix
(define apply-cps
  (lambda (proc args k)
    (k (apply proc args))))

(define length-cps
  (make-cps length))

(define apply-primitive-proc-cps
  (lambda (id args env k)
    (case id
      [(+) (apply-cps + args k)]
      [(-) (apply-cps - args k)]
      [(*) (apply-cps * args k)]
      [(/) (apply-cps / args k)]
      [(add1) (k (+ (car args) 1))]
      [(sub1) (k (- (car args) 1))]
      [(zero?) (k (zero? (car args)))]
      [(not) (k (not (car args)))]
      [(=) (apply-cps = args k)]
      [(<) (apply-cps < args k)]
      [(>) (apply-cps > args k)]
      [(<=) (apply-cps <= args k)]
      [(>=) (apply-cps >= args k)]
      [(cons) (k (cons (car args) (cadr args)))]
      [(car) (k (caar args))]
      [(cdr) (k (cdar args))]
      [(list) (k (apply list args))]
      [(null?) (k (null? (car args)))]
      [(eq?) (k (eq? (car args) (cadr args)))]
      [(equal?) (k(equal? (car args) (cadr args)))]
      [(atom?) (k (atom? (car args)))]
      [(length) (length-cps (car args) k)]
      [(list->vector) (list->vector-cps (car args) k)]
      [(list?) (list?-cps (car args) k)]
      [(pair?) (k (pair? (car args)))]
      [(procedure?) (k (procedure? (car args)))]
      [(vector->list) (vector->list-cps (car args) k)]
      [(vector) (apply-cps vector args k)]
      [(make-vector) (k (make-vector (car args) (cadr args)))]
      [(vector-ref) (k (vector-res (car args) (cadr args)))]
      [(vector?) (k (vector? (car args)))]
      [(number?) (k (number? (car args)))]
      [(symbol?) (k (symbol? (car args)))]
      [(set-car!) (k (set-car! (car args) (cadr args)))]
      [(set-cdr!) (k (set-cdr! (car args) (cadr args)))]
      [(vector-set!) (k (vector-set! (car args) (cadr args) (caddr args)))]
      [(caaar) (k (car (car (car (car args)))))]
      [(caadr) (k (car (car (cdr (car args)))))]
      [(cadar) (k (car (cdr (car (car args)))))]
      [(caddr) (k (car (cdr (cdr (car args)))))]
      [(cdaar) (k (cdr (car (car (car args)))))]
      [(cdadr) (k (cdr (car (cdr (car args)))))]
      [(cddar) (k (cdr (cdr (car (car args)))))]
      [(cdddr) (k (cdr (cdr (cdr (car args)))))]
      [(caar) (k (car (car (car args))))]
      [(cadr) (k (car (cdr (car args))))]
      [(cdar) (k (cdr (car (car args))))]
      [(cddr) (k (cdr (cdr (car args))))]
      [(map) (primitive-map-cps (car args) (cadr args) env k)]
      [(apply) (primitive-apply-cps (car args) (cadr args) env k)]
      [(assq) (primitive-assq-cps (car args) (cadr args) k)]
      [(assv) (primitive-assv-cps (car args) (cadr args) k)]
      [(append) (primitive-append-cps args k)]
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
	   [ref-var (depth position) exp]
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
	   [letrec-exp (bindings body)
		       (app-exp (lambda-exp '() (let ([x (listlength bindings 0)])
					 (begin-exp (add-to-end (map (lambda (binding) (define-exp (var-exp 0 (begin (set! x (- x 1)) x)) (syntax-expand (cadr binding)))) bindings) (syntax-expand body))))) '())]
		       ;(letrec-exp (map (lambda (x) (cons (car x) (cons (syntax-expand (cadr x)) '()))) bindings) (syntax-expand body))]
		       ;(app-exp (lambda-exp (vars-list bindings) (syntax-expand body)) (map syntax-expand (exps-list bindings)))]
	   [named-let (funct vars body)
		      (letrec-exp (cons (cons funct (cons (lambda-exp (vars-list vars) (syntax-expand body)) '())) '()) (app-exp (var-exp 0 0) (map syntax-expand (exps-list vars))))]
		      ;(let ([newvars (append vars (list (list funct (syntaxbody)))])
					;(syntax-expand (letrec-exp newvars (app-exp (var-exp 0 (length vars)) (placevars vars 0)))))]
	   [while-exp (test body) (while-exp (syntax-expand test) (syntax-expand body))]
	   [set-exp (var value) (set-exp var (syntax-expand value))]
	   [define-exp (var value) (define-exp var (syntax-expand value))]
	   [case-exp (value clauses) (case-exp (syntax-expand value) (syntax-expand-clauses clauses))]
	   [cond-exp (exp-pairs) (expand-conds exp-pairs)]
	   [condition-exp (test action) (eopl:error 'expand-syntax "Unhandled condition-exp")]
	   [cond-else (action) (eopl:error 'expand-syntax "Unhandled cond-else")]
	   [app-exp (operator operands)
		    (app-exp (syntax-expand operator)
			  (map syntax-expand operands))]
	   [empty-exp () exp])))

(define expand-conds(
		     lambda(conditions)
		      (cond [(eqv? (caar conditions) 'condition-exp) (if (not (null? (cdr conditions))) (if-exp (syntax-expand (cadar conditions)) (syntax-expand (caddar conditions)) (expand-conds (cdr conditions))) (if-half-exp (syntax-expand (cadar conditions)) (syntax-expand (caddar conditions))))]
			    [(eqv? (caar conditions) 'cond-else) (syntax-expand (cadar conditions))]
			    [else (eopl:error 'expand-conditions "Something went horribly wrong in parsing a cond")]      
			    )
		      )
  )
