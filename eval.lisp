

(defun eval (exp env)
  (cond ((numberp exp) exp)
	((symbolp exp) (lookup exp env))
	((eq (car exp) 'quote) (cadr exp))
	((eq (car exp) 'lambda) `(closure ,(cdr exp) ,env))
	((eq (car exp) 'cond) (evcond (cdr exp) env))
	(t (funcall (apply #'eval (car exp) env)
		    (evlist (cdr exp) env)))
))


#+nil
(cdr (assoc 'exp '((exp . 3) (blub . 4))))

#+nil
(defun lookup (sym env)
  (cdr (assoc sym env)))


(defun evlist (args env)
  (cond ((eq l '()) '())
	(t (cons (eval (car args) env)
		 (eval (cdr args) env)))))

(defun apply (proc args)
  (cond ((primitive-p proc)
	 (apply-primop proc args))
	((eq (car proc) 'closure)
	 ;; procedure has the structure 
	 ;; (closure ((x) (+ x x)) <env>)
	 (eval (cadadr proc) ;; body of procedure
	       (bind (caadr proc) ;; formal parameters
		     args
		     (caddr proc) ;; environment
		     )))
	(t (error "apply"))))


(defun evcond (clauses env)
  (cond ((eq clauses '()) '())
	((eq (caar clauses) 't;; predicate
	     )
	 (eval (cadar clauses) ;; 2nd element of first clause
	       env))
	((false-p (eval (caar clauses) env)) ;; not true, so next
	 (evcond (cdr clauses) env))
	(t (eval cadar clauses) env)))

;; make new frame
(defun bind (vars vals env)
  (cons (pair-up vars vals)
	env))

(defun pair-up (vars vals)
  (cond ((eq vars '())
	 (cond ((eq vals '()) '())
	       (t (error "tka"))))
	((eq vals '())
	 (error "tfa"))
	(t (cons (cons (car vars)
		       (car vals))
		 (pair-up (cdr vars)
			  (cdr vals))))))

(defun lookup (sym env)
  (cond ((eq env '()) (error "ubv"))
	(t (funcall (lambda )))))