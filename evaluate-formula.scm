(define atom?
  (lambda (x)
    (not (pair? x))))

(define evaluate-formula 
  (lambda (expr)
  ;do the regular dispatching here
  expr
))


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

(define traverse-and-args
  (lambda (arg-list)
    arg-list
    ;logical and of all the arguments 
    ;of an and expressions
))
