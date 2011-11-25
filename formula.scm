(define atom?
  (lambda (x)
    (not (pair? x))))


(define formula? 
  (lambda (expr)
    (cond ((atom? expr) (if (atomic-prop? expr) #t #f))
          ((pair? expr) (cond ((unary-prop? expr)
                               (formula? (car (cdr expr))))
                              ((binary-prop? expr) (traverse-args (cdr expr)))
                              (else #f))))))
                             
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
    (if (null? arg-list)
        #t
        (if (formula? (car arg-list))
            (traverse-args (cdr arg-list))
            #f))))
            
   
            
       