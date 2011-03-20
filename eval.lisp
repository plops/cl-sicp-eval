

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
(defun lookup (sym env)
  (cdr (assoc sym env)))
(defun evlist (args env))
(defun apply (proc args)
  (cond ((primitive-p proc)
	 (apply-primop proc args))
	((eq (car proc) 'closure)
	 (eval (cadadr proc)
	       (bind (caadr proc)
		     args
		     (caddr proc))))
	(t (error "apply")))
(defun evcond)