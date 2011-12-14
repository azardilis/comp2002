;;; COMP2002 Coursework 2
;;; Question 4
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define formula->nnf
  (lambda (exp)
    (define atom? 
      (lambda (exp) (not (pair? exp))))
    (define not-expr?
      (lambda (expr) (eq? (car expr) 'not)))
    (define binary-prop?
      (lambda (exp)
        (or (eq? (car exp) 'and)
            (eq? (car exp) 'or))))
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
          (lambda (ex) (eq? (operator exp) (operator (cadr exp)))))
        (define not-operand
          (lambda (exp) (cadr (cadr exp))))
        (define make-or
          (lambda (exp) (cons 'or (make-nots (cdr exp)))))
        (define make-and
          (lambda (exp) (cons 'and (make-nots (cdr exp)))))              
        (define operator
          (lambda (exp) (car exp)))
        (define make-nots 
          (lambda (expr)
            (map (lambda (exp) (list 'not exp)) expr)))
       (cond ((double-negation? exp) (not-operand exp))
             ((not-and? exp) (make-or (cadr exp)))
             ((not-or? exp) (make-and (cadr exp)))
             (else exp))))
    (cond ((atom? exp) exp)
          ((not-expr? exp) (if (atom? (cadr exp)) exp
                               (formula->nnf (normalize exp))))
          ((binary-prop? exp) (cons (car exp) (traverse-args (cdr exp)))))))

;;; Function formula->nnf makes a dispatch on the type of the expression. If it's a binary
;;; proposition(and, or) it calls traverse-args which traverses the arguments sending each one
;;; to formula->nnf. If it's a not expression it gets sent to the normalise function which makes 
;;; a dispatch on the type of the expression. If it's one of the forms that have to be transformed
;;; it does the neseccary actions, otherwise it returns the expressions itself. The results of the 
;;; traversal are consed to forms the final result.