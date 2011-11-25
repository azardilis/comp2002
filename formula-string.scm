(define atom?
  (lambda (x)
    (not (pair? x))))

(define formula->string 
   (lambda (expr)
     (cond ((atom? expr) (symbol->string expr))
           ((not-expr? expr) (if (atom? (car (cdr expr)))
                                 (string-append "~ " (formula->string (cadr expr)))
                                 (string-append "~ (" (formula->string (car (cdr expr))) ")")))
           ((or-expr? expr) (traverse-or-args (cdr expr)))
           ((and-expr? expr) (traverse-and-args (cdr expr))))))

(define not-expr?
  (lambda (expr)
    (and (not (atom? expr))
         (eq? (car expr) 'not)
         (= (length (cdr expr)) 1))))

(define or-expr? 
  (lambda (expr)
    (and (not (atom? expr))
         (eq? (car expr) 'or)
         (> (length (cdr expr)) 1))))


(define and-expr? 
  (lambda (expr)
    (and (not (atom? expr))
         (eq? (car expr) 'and)
         (> (length (cdr expr)) 1))))

(define traverse-or-args
  (lambda (arg-list)
    (if (null? (cdr arg-list))
        (if (pair? (car arg-list))
            (string-append "(" (formula->string(car arg-list)) ")")
            (formula->string(car arg-list)))
        (if (atom? (car arg-list))
                   (string-append (formula->string (car arg-list)) " v " (traverse-or-args (cdr arg-list)))
                   (string-append "(" (formula->string (car arg-list)) ") v " (traverse-or-args (cdr arg-list)))))))

(define traverse-and-args
  (lambda (arg-list)
    (if (null? (cdr arg-list))
        (if (pair? (car arg-list))
            (string-append "(" (formula->string (car arg-list)) ")")
            (formula->string(car arg-list)))
        (if (atom? (car arg-list))
                   (string-append (formula->string (car arg-list)) " ^ " (traverse-and-args (cdr arg-list)))
                   (string-append "(" (formula->string (car arg-list)) ") ^ " (traverse-and-args (cdr arg-list)))))))




