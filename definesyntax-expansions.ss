(define-syntax return-first
        (lambda (x)
                (syntax-case x ()
                        ((_ e0 e1 e2 ...) (syntax (let ([ans e0]) (begin e1 e2 ...) ans)))
                )
        )
)

(define-syntax for
  (syntax-rules ()
    ((for init test update body)
     (begin init (forhelper test update body)))
    )
  )

(define forhelper
  (lambda (test update body)
    (if test
	(let ([evaluation (body)])
	  evaluation
	  (update)
	  (if (test)
	      (forhelper test update body)
	      evaluation)))))