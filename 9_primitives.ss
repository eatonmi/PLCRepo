;Temporary
(define primitive-map-cps
  (lambda (funct slist env k)
    (k (primitive-map funct slist env))))

(define primitive-apply-cps
  (lambda (funct args env k)
    (k (primitive-apply funct args env))))

(define primitive-append-cps
  (lambda (args k)
    (k (primitive-append args))))

(define primitive-assq-cps
  (lambda (get arg k)
    (k (primitive-assq get arg))))

(define primitive-assv-cps
  (lambda (get arg k)
    (k (primitive-assv get arg))))

(define primitive-map(
        lambda(funct slist env)
                (primitive-map-helper funct slist '() env)
        )
)

(define primitive-map-helper(
        lambda(funct slist res env)
                (if (null? slist) res
                        (primitive-map-helper funct (cdr slist)
			(append res (list (apply-proc-cps funct (list (car slist)) (list (car slist)) env (lambda (x) x))) env) env)
                )
        )
)

(define primitive-apply(
        lambda(funct args env)
                (if (null? args) '()
                        (primitive-applyhelper funct (cadr args) 0 env)
                )
        )
)

(define primitive-applyhelper(
        lambda(funct args res env)
                (if (null? args)
		    res
                    (primitive-applyhelper funct (cdr args) (eval-tree-cps (app-exp funct (cons (lit-exp res) (cons (lit-exp (car args)) '()))) env (lambda (x) x)) env)
                )
        )
)

(define primitive-append(
        lambda(args)
                (primitive-appendhelper args '())
       )
)

(define primitive-appendhelper(
        lambda(args res)
                (if (null? args) res
                        (primitive-appendhelper (cdr args) (append res (car args)))
                )
        )
)

(define primitive-assq(
        lambda(get arg)
                (if (null? arg) #f
                        (if (not (pair? (car arg))) (eopl:error 'assq "assq passed a non-association list value")
                                (if (eq? (caar arg) get) (car arg)
                                        (primitive-assq get (cdr arg))
                                )
                        )
                )
        )
)

(define primitive-assv(
        lambda(get arg)
                (if (null? arg) #f
                        (if (not (pair? (car arg))) (eopl:error 'assv "assv passed a non-association list value")
                                (if (eqv? (caar arg) get) (car arg)
                                        (primitive-assv get (cdr arg))
                                )
                        )
                )
        )
)

(define primitive-and(
        lambda(args)
                (if (null? args) #t
                        (andhelper (cdr args) (car args))
                )
        )
)

(define andhelper
  (lambda(args last)
    (if last
	(if (null? args)
	    #t
	    (andhelper (cdr args) (car args)))
	#f)))

(define primitive-or(
        lambda(args)
                (if (null? args) #f
                        (orhelper args #f)
                )
        )
)

(define orhelper(
        lambda(args last)
                (if (null? args) last
                                                (if (not (equal? (car args) #f)) (car args)
                                (orhelper (cdr args) last)
                        )
                )
        )
)
