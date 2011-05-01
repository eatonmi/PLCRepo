(load "chez-init.ss")

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (vals scheme-value?)
   (env environment?)])

(define empty-env
  (lambda ()
    '()))

;(define set-env(
;	lambda(sym val env)
;		(

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
