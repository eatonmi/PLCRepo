(load "chez-init.ss")

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (vals scheme-value?)
   (env environment?)])

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals operands env)
    (cond [(symbol? syms) (cons (cons (cons vals '()) (car env)) (cdr env))]
	  [(null? syms)
	   (if (null? vals)
	       (cons '() env)
	       (eopl:error 'extend-env "Too many values passed to application ~s" vals))]
	  [else (if (not (null? vals))
		    (let ([added (extend-env (cdr syms) (cdr vals) (cdr operands) env)])
		      (if (symbol? (car syms))
			  (cons (cons (car vals) (car added)) (cdr added))
			  (cons (cons (cons 'ref (cons (car operands) '())) (car added)) (cdr added))))
		    (eopl:error 'extend-env "Too few values passed to application"))])))

(define matched?
  (lambda (syms vals)
    (cond [(null? syms) (null? vals)]
	  [(atom? syms) (atom? vals)]
	  [(pair? syms)
	   (if (pair? vals)
	       (matched? (cdr syms) (cdr vals))
	       #f)])))

(define exist-pos?
  (lambda (pos var-ls)
    (if (null? var-ls)
	#f
	(if (eqv? pos 0)
	    #t
	    (get-pos (- pos 1) (cdr var-ls))))))

(define apply-env
  (lambda (env depth position)
    (if (null? env)
	(eopl:error 'apply-env "No bindings for depth ~s" depth)
	(if (zero? depth)
	    (let ([there (exist-pos? position (car env))])
	      (if (not there)
		  (eopl:error 'apply-env "No binding in position ~s" position)
		  (let ([value (get-pos position (car env))])
		    (if (and (list? value)
			     (and (not (null? value))
				  (eqv? (car value) 'ref)))
			(eval-tree (cdr value) env)
			value))))
	    (apply-env (cdr env) (- depth 1) position)))))

(define apply-env-set
  (lambda (env depth position)
    (if (null? env)
	(eopl:error 'apply-env "No bindings for depth ~s" depth)
	(if (zero? depth)
	    (let ([value (get-pos-set position (car env))])
	      (if (not value)
		  (eopl:error 'apply-env "No binding in position ~s" position)
		  (if (and (list? (car value))
			   (and (not (null? (car value)))
				(eqv? (caar value) 'ref)))
		      (let ([exp (cadar value)])
			(if (eqv? (car exp) 'var-exp)
			    (apply-env-set (cdr env) (cadr exp) (caddr exp))
			    (apply-global-set (cadr exp) env)))
		      value)))
	    (apply-env (cdr env) (- depth 1) position)))))

;(define apply-env
 ; (lambda (env depth position)
  ;  (if (null? env)
;	(eopl:error 'apply-env "No bindings for depth ~s" depth)
;	(if (zero? depth)
;	    (let ([there (exist-pos? position (car env))])
;	      (if (not there)
;		  (eopl:error 'apply-env "No binding in position ~s" position)
;		  (get-pos position (car env))))
;	    (apply-env (cdr env) (- depth 1) position)))))

;(define apply-env-set
 ; (lambda (env depth position)
  ;  (if (null? env)
;	(eopl:error 'apply-env "No bindings for depth ~s" depth)
;	(if (zero? depth)
;	    (let ([value (get-pos-set position (car env))])
;	      (if (not value)
;		  (eopl:error 'apply-env "No binding in position ~s" position)
;		  value))
;	    (apply-env-set (cdr env) (- depth 1) position)))))

(define global-primitives '(+ primitive + - * / add1 sub1 zero? not = < > <= >= cons car cdr list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector? number? symbol? set-car! set-cdr! vector-set! caaar caadr cadar caddr cdaar cdadr cddar cdddr caar cadr cdar cddr map apply assq assv append))

(define global '())

(define build-global
  (lambda (primitives-part)
    (if (null? primitives-part)
	'()
	(let ([sym (car primitives-part)])
	  (cons (cons sym (primitive sym)) (build-global (cdr primitives-part)))))))

(define apply-global
  (lambda (sym)
    (apply-global-part sym global)))

(define apply-global-part
  (lambda (sym global-part)
    (cond [(null? global-part) (eopl:error 'apply-global "There is no global binding for ~s" sym)]
	  [(eqv? (caar global-part) sym) (cdar global-part)]
	  [else (apply-global-part sym (cdr global-part))])))

(define apply-global-set
  (lambda (sym)
    (apply-global-set-part sym global)))

(define apply-global-set-part
  (lambda (sym global-part)
    (cond [(null? global-part) (eopl:error 'apply-global "There is no global binding for ~s" sym)]
	  [(eqv? (caar global-part) sym) (car global-part)]
	  [else (apply-global-set-part sym (cdr global-part))])))

(define define-global
  (lambda (sym value)
    (set! global (define-helper sym value global))))

(define define-helper
  (lambda (sym value global-part)
    (cond [(null? global-part) (cons (cons sym value) '())]
	  [(eqv? (caar global-part) sym) (cons (cons sym value) (cdr global-part))]
	  [else (cons (car global-part) (define-helper sym value (cdr global-part)))])))
