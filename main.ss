(load "chez-init.ss")
(load "definesyntax-expansions.ss")
(load "env.ss")
(load "parser.ss")
(load "interpreter.ss")

;<expression>		--> <empty expression>
;			| <variable expression>
;			| <literal>
;			| <begin expression>
;			| <if expression>
;			| <procedure>
;			| <while expression>
;			| <cond>
;			| <let>
;<empty expression>	--> ()
;<variable expression>	--> <symbol>
;<boolean value>		--> <#t> | <#f>
;<quote expression>	--> (quote <expression>)
;<literal>		--> <number> | <string> | <vector> | <boolen value> | <quoted expression>
;<variables>		--> <null> | <symbol> | (<symbol> . <variables>)
;<lambda expression>	--> (lambda <variables> <expression>+)
;<begin expression>	--> (begin <expression>*)
;<if expression>		--> (if <expression> <expression> <expression>) | (if <expression> <expression>)
;<procedure>		--> <lambda expression> | <primitive procedure>
;<primitive procedure>	--> (+ <number>*)
;			| (- <number>+)
;			| (* <number>*)
;			| (/ <number>+)
;			| (add1 <number>)
;			| (sub1 <number>)
;			| (zero? <number>)
;			| (not <expression>)
;			| (= <number>+)
;			| (< <number>+)
;			| (> <number>+)
;			| (<= <number>+)
;			| (>= <number>+)
;			| (cons <expression> <expression>)
;			| (car <pair>)
;			| (cdr <pair>)
;			| (list <expression>*)
;			| (null? <expression>)
;			| (eq? <expression> <expression>)
;			| (equal? <expression> <expression>)
;			| (atom? <expression)
;			| (length <list>)
;			| (list->vector <list>)
;			| (list? <expression>)
;			| (pair <expression>)
;			| (procedure? <expression>)
;			| (vector->list <vector>)
;			| (vector <expression>*)
;			| (make-vector <number>) | (make-vector <number> <expression>)
;			| (vector-ref <vector> <number>)
;			| (number? <expression>)
;			| (symbol? <expression>)
;			| (set-car! <pair> <expression>)
;			| (set-cdr! <pair> <expression>)
;			| (vector-set! <vector> <number> <expression>)
;			| (caaar <pair>)
;			| (caadr <pair>)
;			| (cadar <pair>)
;			| (caddr <pair>)
;			| (cdaar <pair>)
;			| (cdadr <pair>)
;			| (cddar <pair>)
;			| (cdddr <pair>)
;			| (caar <pair>)
;			| (cadr <pair>)
;			| (cdar <pair>)
;			| (cddr <pair>)
;			| (and <variables>)
;			| (or <variables>)
;
;<cond>			--> <cond-exp> | <condition-exp> | <cond-else>
;<cond-exp>		--> (cond <condtions>)
;<conditions> 		--> <condition-exp> <conditions> | <condition-exp> <cond-else> | <empty expression>
;<condition-exp>		--> (<expression> <expression>)
;<cond-else>		--> (else <expression>)
;<while expression>	--> (while <expression> <expression>)
;<let>			--> <let-exp> | <let*-exp>
;<let-exp>		--> (let (<variable-assignments>) <expression>)
;<let*-exp>		--> (let (<variable-assignments>) <expression?)
;<variable-assignments>	--> <variable-assignment> | <variable-assignment> <variable-assignments>
;<variable-assignment> 	--> (<symbol> <expression>)


(define (rl) (load "int.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([foo (read)])
      (if (not (equal? foo '(exit)))
	  (begin (write (interpret foo))
		 (newline)
		 (rep))))))
     
(define eval-one-exp
  (lambda (exp)
    (interpret exp)))
