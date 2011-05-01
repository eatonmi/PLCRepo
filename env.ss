(load "chez-init.ss")

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (vals scheme-value?)
   (env environment?)])

(define empty-env
  (lambda ()
    '()))

(define set-env(
	lambda(sym val env)
		(if (equal? env (empty-env)) (set-global sym val)
			(let* ([current-vals (caar env)]
		      	      [current-syms (cadar env)]
			      [position (find-pos sym current-syms)])
		  	 (if position (cons (list (set-val-pos val position current-vals) current-syms) (cdr env))
			   (set-env sym val (cdr env)))))))

(define set-val-pos(
	lambda(val position current)
		(if (null? current) (eopl:error 'set-env "Error in find-pos caused set-val-pos to try and set a non-existent variable")
		(if (eq? position 0) (cons val (cdr current))
		  (cons (car current) (set-val-pos val (- position 1) (cdr current)))))))

(define set-global(
	lambda(sym val)
		(eopl:error 'set-global "Global setting not yet implemented")
	)
 )

(define extend-env
  (lambda (syms vals env)
    (cond [(symbol? syms) (cons (cons vals '()) env)]
	  [(null? syms)
	   (if (null? vals)
	       (cons '(() ()) env)
	       (eopl:error 'extend-env "Too many values passed to application ~s" vals))]
	  [else (if (not (null? vals))
		    (let ([added (extend-env (cdr syms) (cdr vals) env)])
		      (cons (list (cons (car vals) (caar added)) (cons (car syms) (cadar added))) (cdr added)))
		    (eopl:error 'extend-env "Too few values passed to application"))])))

(define matched?
  (lambda (syms vals)
    (cond [(null? syms) (null? vals)]
	  [(atom? syms) (atom? vals)]
	  [(pair? syms)
	   (if (pair? vals)
	       (matched? (cdr syms) (cdr vals))
	       #f)])))

(define apply-env
  (lambda (env depth position)
    (if (null? env)
	(eopl:error 'apply-env "No bindings for depth ~s" depth)
	(if (zero? depth)
	    (let ([value (get-pos position (caar env))])
	      (if (not value)
		  (eopl:error 'apply-env "No binding in position ~s" position)
		  value))
	    (apply-env (cdr env) (- depth 1) position)))))

(define apply-env-set
  (lambda (env depth position)
    (if (null? env)
	(eopl:error 'apply-env "No bindings for depth ~s" depth)
	(if (zero? depth)
	    (let ([value (get-pos-set position (caar env))])
	      (if (not value)
		  (eopl:error 'apply-env "No binding in position ~s" position)
		  value))
	    (apply-env (cdr env) (- depth 1) position)))))

(define global '(+ - * / and or add1 sub1 zero? not = < > <= >= cons car cdr list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector? number? symbol? set-car! set-cdr! vector-set! caaar caadr cadar caddr cdaar cdadr cddar cdddr caar cadr cdar cddr map apply assq assv append))
