;;seems to work okay
;;works in o(n) probably because of the flatten thingie which runs in n because it only uses cons which is constant
;;if we assume that list works in constant
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

(define fstr
  (lambda (exp)
    (list->string (flatten (formula->string exp)))))

(define formula->string 
   (lambda (expr)
     (cond ((atom? expr) (symbol->upstring expr))
           ((not-expr? expr) (not-argument expr))
           ((or-expr? expr) (traverse-or-args (cdr expr)))
           ((and-expr? expr) (traverse-and-args (cdr expr))))))

(define symbol->upstring
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
    (cond ((or (atom? (cadr expr))
               (not-expr? (cadr expr)))
           (cons #\̃ (formula->string (cadr expr))))
          (else (list #\̃ #\( (formula->string (car (cdr expr))) #\))))))
  
(define traverse-or-args
  (lambda (arg-list)
    (cond ((null? (cdr arg-list))
           (if (or (atom? (car arg-list))
                   (not-expr? (car arg-list)))
               (formula->string(car arg-list))
            (list #\( (formula->string (car arg-list)) #\))))
          ((or (atom? (car arg-list))
               (not-expr? (car arg-list)))
           (list (formula->string (car arg-list)) #\space #\v #\space (traverse-or-args (cdr arg-list))))
          (else (list #\( (formula->string (car arg-list)) #\) #\space #\v #\space (traverse-or-args (cdr arg-list)))))))

(define traverse-and-args
  (lambda (arg-list)
    (cond ((null? (cdr arg-list))
           (if (or (atom? (car arg-list))
                   (not-expr? (car arg-list)))
            (formula->string(car arg-list))
            (list #\( (formula->string (car arg-list)) #\))))
          ((or (atom? (car arg-list))
               (not-expr? (car arg-list)))
           (list (formula->string (car arg-list)) #\space #\^ #\space (traverse-and-args (cdr arg-list))))
          (else (list #\( (formula->string (car arg-list)) #\) #\space #\^ #\space (traverse-and-args (cdr arg-list)))))))
