(load "chez-init.ss")
;;;CPS'd
(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (vals scheme-value?)
   (env environment?)])
;;;CPS'd
(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals operands env env2)
    (extend-envCPS syms vals operands env env2 (lambda (x) x))))

(define extend-envCPS
  (lambda (syms vals operands env env2 k)
    (cond [(symbol? syms) (k (cons (cons (cons vals '()) (car env)) (cdr env)))]
	  [(null? syms)
	   (if (null? vals)
	       (k (cons '() env))
	       (eopl:error 'extend-env "Too many values passed to application ~s" vals))]
	  [else (if (not (null? vals))
		  (extend-envCPS (cdr syms) (cdr vals) (cdr operands) env env2 
		    (lambda (hole)
		      (if (symbol? (car syms))
			(k (cons (cons (car vals) (car hole)) (cdr hole)))
			(let ([exp (car operands)])
			  (if (or (eqv? (car exp) 'var-exp)
				  (eqv? (car exp) 'free-exp))
			    (k (cons (cons (cons 'ref (cons exp (cons env2 '()))) (car hole)) (cdr hole)))
			    ;;;This line will need to be modified once a CPS'd version of eval-tree is available
			    (k (cons (cons (eval-tree exp env2) (car hole)) (cdr hole))))))))
		  (eopl:error 'extend-env "Too few values passed to application"))])))
				
;;;Non-CPS version.  Replaces everything after the [else ... portion
;		    (let ([added (extend-env (cdr syms) (cdr vals) (cdr operands) env env2)])
;		      (if (symbol? (car syms))
;			  (cons (cons (car vals) (car added)) (cdr added))
;			  (let ([exp (car operands)])
;			    (if (or (eqv? (car exp) 'var-exp)
;				    (eqv? (car exp) 'free-exp))
;				(cons (cons (cons 'ref (cons exp (cons env2 '()))) (car added)) (cdr added))
;				(cons (cons (eval-tree exp env2) (car added)) (cdr added))))))
;		    (eopl:error 'extend-env "Too few values passed to application"))])))

;;;CPS'd
(define matched?
  (lambda (syms vals)
    (matched?cps syms vals (lambda (x) x))))
;;;CPS'd
(define matched?cps
  (lambda (syms vals k)
    (cond [(null? syms) (k (null? vals))]
	  [(atom? syms) (k (atom? vals))]
	  [(pair? syms)
	   (if (pair? vals)
	       (matched?cps (cdr syms) (cdr vals) k)
	       (k #f))])))
;;;CPS'd
(define exist-pos?(
	lambda(pos var-ls)
	 (exist-pos?cps pos var-ls (lambda (x) x))))
;;;CPS'd
(define exist-pos?cps
  (lambda (pos var-ls k)
    (if (null? var-ls)
	(k #f)
	(if (eqv? pos 0)
	    (k #t)
	    (exist-pos?cps (- pos 1) (cdr var-ls) k)))))

;;;CPS Wrapper
(define apply-env(
	lambda(env depth position)
	 (apply-envCPS env depth position (lambda (x) x))))

;;;Still needs get-pos CPS'ified
(define apply-envCPS
  (lambda (env depth position k)
    (if (null? env)
	(eopl:error 'apply-env "No bindings for depth ~s" depth)
	(if (zero? depth)
	  (getposcps position (car env) 
		(lambda (hole)
		  (if hole
		   (if (and (list? hole) (and (not (null? hole)) (eqv? (car hole) 'ref)))
				       ;;;When eval-tree gets CPS, use this line rather than the following:
				       (eval-tree-cps (cadr hole) (caddr hole) k)
		     		       ;;;Line applied
				       ;;(eval-tree (cadr hole) (caddr hole))
				       (k hole))))) 
		    (apply-envCPS (cdr env) (- depth 1) position k)))))

;;;Non-CPS implementation
;	    (let ([there (exist-pos? position (car env))])
;	      (if (not there)
;		  (eopl:error 'apply-env "No binding in position ~s" position)
;		  (let ([value (get-pos position (car env))])
;		    (if (and (list? value)
;			     (and (not (null? value))
;				  (eqv? (car value) 'ref)))
;			(eval-tree (cadr value) (caddr value))
;			value))))
;;;	    (apply-env (cdr env) (- depth 1) position)))))


;;;Non-CPS implementation
;(define apply-env-set
;  (lambda (env depth position)
;    (if (null? env)
;	(eopl:error 'apply-env "No bindings for depth ~s" depth)
;	(if (zero? depth)
;	    (let ([value (get-pos-set position (car env))])
;	      (if (not value)
;		  (eopl:error 'apply-env "No binding in position ~s" position)
;		  (if (and (list? (car value))
;			   (and (not (null? (car value)))
;				(eqv? (caar value) 'ref)))
;		      (let ([exp (cadar value)])
;			(if (eqv? (car exp) 'var-exp)
;			    (apply-env-set (caddar value) (cadr exp) (caddr exp))
;			    (apply-global-set (cadr exp))))
;		      value)))
;	    (apply-env-set (cdr env) (- depth 1) position)))))
;;;CPS wrapper
(define apply-env-set(
	lambda (env depth position)
		(apply-env-setCPS env depth position (lambda (x) x))))

(define apply-env-setCPS(
	lambda(env depth position k)
	     (if (null? env)
	       (eopl:error 'apply-env "No bindings for depth ~s" depth)
	       (if (zero? depth)
		 (get-pos-setCPS position (car env) 
			(lambda (hole)
			  (if hole
			    (if (and (list? hole) (and (not (null? hole)) (eqv? (car hole) 'ref)))
				(let ([exp (cadar hole)])
				  (if (eqv? (car exp) 'var-exp)
				      (apply-env-setCPS (caddar hole) (cadr exp) (caddr exp) k)
				      (apply-global-setCPS (cadr exp) k)))
				(k hole)))))
		 (apply-env-setCPS (cdr env) (- depth 1) position k)))))


;;;Non-CPS implementations
;(define apply-env
;  (lambda (env depth position)
;    (if (null? env)
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
;;CPS'd
;;NOTE:  "primitive" removed from list
(define global-primitives '(+ - * / add1 sub1 zero? not = < > <= >= cons car cdr list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector? number? symbol? set-car! set-cdr! vector-set! caaar caadr cadar caddr cdaar cdadr cddar cdddr caar cadr cdar cddr map apply assq assv append))
;;;CPS'd
(define global '())

;;;CPS Wrapper
;(define build-global
 ; (lambda (primitives-part)
  ;  (build-globalCPS primitives-part (lambda (x) x))))

;;;CPS-atized
(define build-global
  (lambda (primitives-part k)
    (if (null? primitives-part)
	(k '())
	(let ([sym (car primitives-part)])
	  (build-global (cdr primitives-part) (lambda (x) (k (cons (cons sym (cons (primitive sym) '())) x))))))))

;;;CPS wrapper
(define apply-global
  (lambda (sym k)
    (apply-global-part sym global k)))
;;;CPS-atized
(define apply-global-part
  (lambda (sym global-part k)
    (cond [(null? global-part) (eopl:error 'apply-global "There is no global binding for ~s" sym)]
	  [(eqv? (caar global-part) sym) (k (cadar global-part))]
	  [else (apply-global-part sym (cdr global-part) k)])))
;;;CPS wrapper
(define apply-global-set
  (lambda (sym k)
    (apply-global-setCPS sym global k)))
;;;CPS-atized
(define apply-global-setCPS
  (lambda (sym global-part k)
    (cond [(null? global-part) (eopl:error 'apply-global "There is no global binding for ~s" sym)]
	  [(eqv? (caar global-part) sym) (k (cdar global-part))]
	  [else (apply-global-setCPS sym (cdr global-part) k)])))
;;;CPS-atized
(define define-global
  (lambda (sym value k)
    (set! global (define-helper sym value global k))))

;;;CPS-atized
(define define-helper
  (lambda (sym value global-part k)
   (cond [(null? global-part) (k (cons (cons sym (cons value '())) '()))]
	  [(eqv? (caar global-part) sym) (k (cons (cons sym (cons value '())) (cdr global-part)))]
	  [else (define-helper sym value (cdr global-part) (lambda (x) (k (cons (car global-part) x))))])))
