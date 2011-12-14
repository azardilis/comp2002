;;; COMP2002 Coursework 2
;;; Question 3
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define evaluate-formula 
  (lambda (expr alist)
    (define atom?
      (lambda (x) (not (pair? x))))
    (define not-expr?
      (lambda (expr) (eq? (car expr) 'not)))
    (define or-expr? 
      (lambda (expr) (eq? (car expr) 'or)))        
    (define and-expr? 
      (lambda (expr) (eq? (car expr) 'and)))
    (define find-binding
      (lambda (symbol alist) (cdr (assq symbol alist))))
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
    (cond ((atom? expr) (find-binding expr alist))
          ((not-expr? expr) (not (evaluate-formula (cadr expr) alist)))
          ((or-expr? expr) (traverse-or-args (cdr expr) alist))
          ((and-expr? expr) (traverse-and-args (cdr expr) alist)))))

;;; Function evaluate-formula makes a dispatch on the type of the expression.
;;; If the expression is an and-expression or an or-expression its arguments  
;;; are traversed and their results are combined with logical "and" and "or"
;;; respectively. To get the results of the expression, when a symbol is reached
;;; its value is looked up in the a-list provided as an argument.


