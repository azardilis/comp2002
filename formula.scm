(define atom?
  (lambda (x)
    (not (pair? x))))

(define formula? 
  (lambda (expr)
    (cond ((atom? expr) (atomic-prop? expr))
          ((unary-prop? expr) (formula? (cadr expr)))
          ((binary-prop? expr) (traverse-args (cdr expr))))))
                            
(define atomic-prop?
  (lambda (expr)
    (and (atom? expr)
         (symbol? expr)
         (not (or (eq? expr 'and)
                  (eq? expr 'or)
                  (eq? expr 'not)))
         (not (null? expr)))))

(define unary-prop?
  (lambda (expr)
    (and (eq? (car expr) 'not)
         (= (length (cdr expr)) 1))))

(define binary-prop?
  (lambda (expr)
    (and (or (eq? (car expr) 'and)
             (eq? (car expr) 'or))
         (> (length (cdr expr)) 1))))

(define traverse-args
  (lambda (arg-list)
    (cond ((null? arg-list) #t)
          ((formula? (car arg-list))
           (traverse-args (cdr arg-list)))
          (else #f))))
       
   
            
       