;;; COMP2002 Coursework 2
;;; Question 3
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define evaluate-formula 
  (lambda (expr alist)
    (define atom?
      (lambda (x)
        (not (pair? x))))
    (define not-expr?
      (lambda (expr)
        (eq? (car expr) 'not)))
    (define or-expr? 
      (lambda (expr)
        (eq? (car expr) 'or)))    
    (define and-expr? 
      (lambda (expr)
        (eq? (car expr) 'and)))
    (cond ((atom? expr) (find-binding expr alist))
          ((not-expr? expr) (not (evaluate-formula (cadr expr) alist)))
          ((or-expr? expr) (traverse-or-args (cdr expr) alist))
          ((and-expr? expr) (traverse-and-args (cdr expr) alist)))))

(define find-binding
  (lambda (symbol alist)
    (define (get-symbol cdr)
      (get-symbol (assq symbol alist))))
  
(define traverse-and-args
  (lambda (arg-list alist)
    (cond ((null? arg-list) #t)
          ((evaluate-formula (car arg-list) alist) 
           (traverse-and-args (cdr arg-list) alist))
          (else #f))))

(define traverse-or-args
  (lambda (arg-list alist)
    (cond ((null? arg-list) #f)
          ((evaluate-formula (car arg-list) alist) #t)
          (else (traverse-or-args(cdr arg-list) alist)))))

;;;comments on my solution
;;;
;;;


