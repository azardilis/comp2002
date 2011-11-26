(define atom?
  (lambda (x)
    (not (pair? x))))

(define evaluate-formula 
  (lambda (expr alist)
    (cond ((atom? expr) (find-binding expr alist))
          ((not-expr? expr) (not (evaluate-formula (cadr expr) alist)))
          ((or-expr? expr) (traverse-or-args (cdr expr) alist))
          ((and-expr? expr) (traverse-and-args (cdr expr) alist)))))


;given a symbol and an a-list, should return its binding in the list
(define find-binding 
  (lambda (symbol alist)
    (if (null? alist)
        -1
        (if (eq? (caar alist) symbol)
            (cdr (car alist))
            (find-binding symbol (cdr alist))))))

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

;traverses the arguments and applies logical and to them
(define traverse-and-args
  (lambda (arg-list alist)
    (if (null? arg-list)
        #t
        (if (evaluate-formula (car arg-list) alist)
            (traverse-and-args(cdr arg-list) alist)
            #f))))

;traverses the arguments and applies logical or to them
(define traverse-or-args 
  (lambda (arg-list alist)
    (if (null? arg-list)
        #f
        (if (evaluate-formula (car arg-list) alist)
            #t
            (traverse-or-args(cdr arg-list) alist)))))
