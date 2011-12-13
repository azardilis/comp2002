;;; COMP2002 Coursework 2
;;; Question 2
;;; Argyris Zardilis az2g10@ecs.soton.ac.uk

(define formula->string
  (lambda (exp)
    (list->string (flatten (exp->charlist exp)))))

(define exp->charlist
   (lambda (expr)
     (cond ((atom? expr) (symbol->upchars expr))
           ((not-expr? expr) (not-argument expr))
           ((or-expr? expr) (traverse-or-args (cdr expr)))
           ((and-expr? expr) (traverse-and-args (cdr expr))))))

(define symbol->upchars
  (lambda (s)
    (define charlist->upper
      (lambda (l)
        (if (null? l)
            '()
            (cons (char-upcase (car l))
                  (charlist->upper (cdr l))))))
    (charlist->upper (string->list (symbol->string s)))))
    
(define (flatten char-list)
  (define (flatten-helper char-list)
    (if (atom? char-list) char-list
      (cond ((null? (car char-list)) (flatten-helper (cdr char-list)))
            ((atom? (car char-list)) (cons (car char-list) (flatten-helper (cdr char-list))))
            (else(flatten-helper (cons (caar char-list) (cons (cdar char-list) (cdr char-list))))))))
  (flatten-helper char-list))

(define not-argument
  (lambda (expr)
    (define no-brackets-not?
      (lambda (exp)
        (or (atom? exp)
            (not-expr? exp))))
    (cond ((no-brackets-not? (cadr expr))
           (cons #\̃ (exp->charlist (cadr expr))))
          (else (list #\̃ #\( (exp->charlist (car (cdr expr))) #\))))))

(define traverse-or-args
  (lambda (arg-list)
    (define no-brackets-or?
      (lambda (exp)
        (or (atom? exp)
            (not-expr? exp)
            (or-expr? exp)
            (and (binary-prop? exp)
                 (null? (cddr exp))))))
    (cond ((null? (cdr arg-list))
           (if (no-brackets-or? (car arg-list))
               (exp->charlist(car arg-list))
               (list #\( (exp->charlist (car arg-list)) #\))))
          ((no-brackets-or? (car arg-list))
           (list (exp->charlist (car arg-list)) #\space #\v #\space (traverse-or-args (cdr arg-list))))
          (else (list #\( (exp->charlist (car arg-list)) #\) #\space #\v #\space (traverse-or-args (cdr arg-list)))))))

(define traverse-and-args
  (lambda (arg-list)
    (define no-brackets-and?
      (lambda (exp)
        (or (atom? exp)
            (not-expr? exp)
            (and-expr? exp)
            (and (binary-prop? exp)
                 (null? (cddr exp))))))
    (cond ((null? (cdr arg-list))
           (if (no-brackets-and? (car arg-list))
            (exp->charlist(car arg-list))
            (list #\( (exp->charlist (car arg-list)) #\))))
          ((no-brackets-and? (car arg-list))
           (list (exp->charlist (car arg-list)) #\space #\^ #\space (traverse-and-args (cdr arg-list))))
          (else (list #\( (exp->charlist (car arg-list)) #\) #\space #\^ #\space (traverse-and-args (cdr arg-list)))))))

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

(define binary-prop?
  (lambda (exp)
    (or (eq? (car exp) 'and)
        (eq? (car exp) 'or))))

;;;comments on my solution
;;;
;;;

