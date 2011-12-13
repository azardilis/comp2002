(define atom?
  (lambda (x)
    (not (pair? x))))

(define formula? 
  (lambda (expr)
    (cond ((atom? expr) (atomic-prop? expr))
          ((unary-prop? expr) (formula? (cadr expr)))
          ((binary-prop? expr) (traverse-args (cdr expr)))
          (else #f))));if we reached this point it means that it's not a wff
                            
(define atomic-prop?
  (lambda (expr)
    (and (symbol? expr)
         (not (or (eq? expr 'and)
                  (eq? expr 'or)
                  (eq? expr 'not)))
         (not (null? expr)))))

(define unary-prop?
  (lambda (expr)
    (and (eq? (car expr) 'not)
         (if (null? (cdr expr)) #f
             (null? (cddr expr))))))

(define binary-prop?
  (lambda (expr)
    (and (or (eq? (car expr) 'and)
             (eq? (car expr) 'or))
         (not (null? (cdr expr))))))

(define traverse-args
    (lambda (arg-list)
      (cond ((null? arg-list) #t)
            ((formula? (car arg-list))
                (traverse-args (cdr arg-list)))
            (else #f))))