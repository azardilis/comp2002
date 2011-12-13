;;; COMP2002 Coursework 2
;;; Question 1
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define atom?
  (lambda (x)
    (not (pair? x))))

(define formula? 
  (lambda (expr)
    (define not-wff #f)
    (cond ((atom? expr) (atomic-prop? expr))
          ((unary-prop? expr) (formula? (cadr expr)))
          ((binary-prop? expr) (traverse-args (cdr expr)))
          (else not-wff))))
                            
(define atomic-prop?
  (lambda (expr)
    (and (symbol? expr)
         (not (or (eq? expr 'and)
                  (eq? expr 'or)
                  (eq? expr 'not)))
         (not (null? expr)))))

(define unary-prop?
  (lambda (expr)
    (define not-wff #f)
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
      (define wff #t)
      (define not-wff #f)
      (cond ((null? arg-list) wff)
            ((formula? (car arg-list))
                (traverse-args (cdr arg-list)))
            (else not-wff))))

;;;comments on my solution
;;;
;;;