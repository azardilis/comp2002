;assume that it is a well-formed formula(from spec) and all the vars have a binding in the a-list provided
(define atom?
  (lambda (x)
    (not (pair? x))))

;does the regular dispatching depending on the type of the expression
(define evaluate-formula 
  (lambda (expr alist)
    (cond ((atom? expr) (find-binding expr alist))
          ((not-expr? expr) (not (evaluate-formula (cadr expr) alist)))
          ((or-expr? expr) (traverse-or-args (cdr expr) alist))
          ((and-expr? expr) (traverse-and-args (cdr expr) alist)))))

;finds the binding of a symbol in the list
;assume it always finds it!
(define find-binding
  (lambda (symbol alist)
    (cdr (assq symbol alist))))

;finds the binding of a symbol in the list
;assume it always finds it!
;(define find-binding 
;  (lambda (symbol alist)
;    (cond ((null? alist) -1)
;          ((eq? (caar alist) symbol) 
;           (cdr (car alist)))
;          (else (find-binding symbol (cdr alist))))))

(define not-expr?
  (lambda (expr)
    (eq? (car expr) 'not)))

(define or-expr? 
  (lambda (expr)
    (eq? (car expr) 'or)))
        
(define and-expr? 
  (lambda (expr)
    (eq? (car expr) 'and)))

(define traverse-and-args
  (lambda (arg-list alist)
    (cond ((null? arg-list) #t)
          ((evaluate-formula (car arg-list) alist) 
           (traverse-and-args (cdr arg-list) alist))
          (else #f))))

(define traverse-or-args
  (lambda (arg-list alist)
    (cond ((null? arg-list) #f)
          ((evaluate-formula (car arg-list) alist) #t)
          (else (traverse-or-args(cdr arg-list) alist)))))

