(define atom?
  (lambda (x)
    (not (pair? x))))

(define formula->string 
   (lambda (expr)
     (cond ((atom? expr) (sexp->upper expr))
           ((not-expr? expr) (not-argument expr))
           ((or-expr? expr) (traverse-or-args (cdr expr)))
           ((and-expr? expr) (traverse-and-args (cdr expr))))))

(define sexp->upper
  (lambda (exp)
    (list->string (clist->upper (string->list (symbol->string exp))))))

(define clist->upper
  (lambda (clist)
    (if (null? clist)
        '()
        (cons (char-upcase (car clist))
              (clist->upper (cdr clist))))))

(define not-expr?
  (lambda (expr)
    (eq? (car expr) 'not)))

(define or-expr? 
  (lambda (expr)
    (eq? (car expr) 'or)))
        
(define and-expr? 
  (lambda (expr)
    (eq? (car expr) 'and)))

(define not-argument
  (lambda (expr)
    (cond ((atom? (cadr expr)) 
           (string-append "̃" (formula->string (cadr expr))))
          (else (string-append "̃ (" (formula->string (car (cdr expr))) ")")))))
  
(define traverse-or-args
  (lambda (arg-list)
    (cond ((null? (cdr arg-list))
           (if (pair? (car arg-list))
            (string-append "(" (formula->string (car arg-list)) ")")
            (formula->string(car arg-list))))
          ((or (atom? (car arg-list))
               (not-expr? (car arg-list)))
           (string-append (formula->string (car arg-list)) " v " (traverse-and-args (cdr arg-list))))
          (else (string-append "(" (formula->string (car arg-list)) ") v " (traverse-and-args (cdr arg-list)))))))

(define traverse-and-args
  (lambda (arg-list)
    (cond ((null? (cdr arg-list))
           (if (pair? (car arg-list))
            (string-append "(" (formula->string (car arg-list)) ")")
            (formula->string(car arg-list))))
          ((or (atom? (car arg-list))
               (not-expr? (car arg-list)))
           (string-append (formula->string (car arg-list)) " ^ " (traverse-and-args (cdr arg-list))))
          (else (string-append "(" (formula->string (car arg-list)) ") ^ " (traverse-and-args (cdr arg-list)))))))
