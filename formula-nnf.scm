;;; COMP2002 Coursework 2
;;; Question 4
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define formula->nnf
  (lambda (exp)
    (cond ((atom? exp) exp)
          ((not-expr? exp) (if (atom? (cadr exp)) exp
                               (formula->nnf (normalize exp))))
          ((binary-prop? exp) (cons (car exp) (traverse-args (cdr exp)))))))

(define traverse-args
  (lambda (arg-list)
    (cond ((null? arg-list) '())
          (else (cons (formula->nnf (car arg-list)) 
                      (traverse-args (cdr arg-list)))))))

(define normalize
  (lambda (exp)
    (define not-or?
      (lambda (exp)
        (cond ((binary-prop? (cadr exp))
               (eq? (caadr exp) 'or))
              (else #f))))
    (define not-and?
      (lambda (exp)
        (cond ((binary-prop? (cadr exp))
               (eq? (caadr exp) 'and))
              (else #f))))
    (define double-negation? 
      (lambda (ex) (eq? (operator exp) (car (cadr exp)))))
    (define not-operand
      (lambda (exp) (cadr (cadr exp))))
    (define make-or
      (lambda (exp) (cons 'or (make-nots (cdr exp)))))
    (define make-and
      (lambda (exp) (cons 'and (make-nots (cdr exp)))))              
    (define operator
      (lambda (exp) (car exp)))
    (define make-nots
      (lambda (exp)
        (cond ((null? exp) '())
          (else (cons (list 'not (car exp))
                      (make-nots (cdr exp)))))))
   (cond ((double-negation? exp) (not-operand exp))
         ((not-and? exp) (make-or (cadr exp)))
         ((not-or? exp) (make-and (cadr exp)))
         (else exp))))

(define atom? 
  (lambda (exp) (not (pair? exp))))

(define not-expr?
  (lambda (expr) (eq? (car expr) 'not)))

(define binary-prop?
  (lambda (exp)
    (or (eq? (car exp) 'and)
        (eq? (car exp) 'or))))
;;;comment on my solution
;;;
;;;