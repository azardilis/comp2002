;passes first tests have to do some further testing though
; and make it prettier:p

(define atom? 
  (lambda (exp)
    (not (pair? exp))))

(define formula->nnf
  (lambda (exp)
    (cond ((atomic-prop? exp) exp)
          ((unary-prop? exp) (if (atom? (cadr exp)) exp
                                 (formula->nnf (normalize exp))))
          ((binary-prop? exp) (cons (car exp) (traverse-args (cdr exp)))))))

(define unary-prop?
  (lambda (expr)
    (and (eq? (car expr) 'not)
         (= (length (cdr expr)) 1))))

(define binary-prop?
  (lambda (expr)
    (or (eq? (car expr) 'and)
        (eq? (car expr) 'or))))
           
(define atomic-prop?
  (lambda (expr)
    (and (atom? expr)
         (symbol? expr)
         (not (or (eq? expr 'and)
                  (eq? expr 'or)
                  (eq? expr 'not)))
         (not (null? expr)))))

(define traverse-args
  (lambda (arg-list)
    (cond ((null? arg-list) '())
          (else (cons (formula->nnf (car arg-list)) 
                      (traverse-args (cdr arg-list)))))))

(define normalize
  (lambda (exp)
    (cond ((double-negation? exp) (not-operand exp))
          ((not-and? exp) (make-or (cadr exp)))
          ((not-or? exp) (make-and (cadr exp)))
          (else exp))))

;case: not followed by or expression
(define not-or?
  (lambda (exp)
    (cond ((binary-prop? (cadr exp))
           (eq? (caadr exp) 'or))
          (else #f))))

;case : not followed by and expression 
(define not-and?
  (lambda (exp)
    (cond ((binary-prop? (cadr exp))
           (eq? (caadr exp) 'and))
          (else #f))))

;case: not followed by not
(define double-negation? 
  (lambda (exp)
    (eq? (operator exp) (car (cadr exp)))))

(define not-operand
  (lambda (exp)
    (cadr (cadr exp))))

(define make-or
  (lambda (exp)
    (cons 'or (make-nots (cdr exp)))))

(define make-and
  (lambda (exp)
    (cons 'and (make-nots (cdr exp)))))
                      
(define operator
  (lambda (exp)
    (car exp)))

(define make-nots
  (lambda (exp)
    (cond ((null? exp) '())
          (else (cons (list 'not (car exp))
                      (make-nots (cdr exp)))))))