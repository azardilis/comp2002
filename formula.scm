;;; COMP2002 Coursework 2
;;; Question 1
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define formula? 
  (lambda (expr)
    (define not-wff #f)
    (define wff #t)
    (define atom?
      (lambda (x) (not (pair? x))))
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
             (if (null? (cdr expr)) not-wff
                 (null? (cddr expr))))))
    (define binary-prop?
      (lambda (expr)
        (and (or (eq? (car expr) 'and)
                 (eq? (car expr) 'or))
             (not (null? (cdr expr))))))
    (define traverse-args
      (lambda (arg-list)
        (cond ((null? arg-list) wff)
              ((formula? (car arg-list))
               (traverse-args (cdr arg-list)))
              (else not-wff))))
    (cond ((atom? expr) (atomic-prop? expr))
          ((unary-prop? expr) (formula? (cadr expr)))
          ((binary-prop? expr) (traverse-args (cdr expr)))
          (else not-wff))))
                            
;;; Function formula? starts with a dispatch on the type of the expression.
;;; If the expression is a unary proposition it continues by sending the argument
;;; to formula? to be checked if it's a wff. If it's a binary proposition (and, or)
;;; it might have arbitrary amount of arguments so the arguments list is sent to traverse-args.
;;; In function traverse-args each argument is sent to formula? again to be checked. The results
;;; of all the arguments are combined with logical "and" in the traversal so that means that only
;;; if all the arguments are well formed the whole formula is regarded as well formed. If the expression
;;; is not recognised to be one of the correct atom, unary-prop,binary-prop is immediately regarded
;;; as not well formed.