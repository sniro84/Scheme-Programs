; 12.7: Length of a list.
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

; 12.8: Multiplication.
(define mult
  (lambda (lst)
    (if (null? lst)
        1
        (* (car lst) (mult (cdr lst))))))

; 12.9: Factorial.
(define fact
  (lambda (n)
    (if (= n 0)
        1
        (* n (fact (- n 1))))))

; 12.10: Factorial with list.
(define fact-list
  (lambda (lst)
    (if (null? lst)
        null
        (cons (fact (car lst)) (fact-list (cdr lst))))))

; 12.11: Member.
(define member?
  (lambda (x lst)
    (if (null? lst)
        #f
        (if (equal? x (car lst))
            #t
            (member? x (cdr lst))))))

; 12.12: Odd/Even - for positive numbers only
(define even?
  (lambda (n)
    (if (equal? n 0)
        #t
        (odd? (- n 1)))))
(define odd?
  (lambda (n)
    (if (equal? n 0)
        #f
        (even? (- n 1)))))

; 12.13 Remove-First
(define remove-first
  (lambda (x lst)
    (cond ((null? lst) '())
          ((equal? (car lst) x) (cdr lst))
          (else (cons (car lst) (remove-first x (cdr lst)))))))

; 12.14 Remove-All
(define remove
  (lambda (x lst)
    (if (equal? (member? x lst) #f)
        lst
        (remove x (remove-first x lst)))))

; 12.15 firsts
(define firsts
  (lambda (lst)
    (if (null? lst)
        '() 
        (cons (caar lst) (firsts (cdr lst))))))

; 12.16 insertR
(define insertR
  (lambda (old new lst)
     (if (equal? (car lst) old)
        (cons old (cons new (cdr lst)))
        (cons (car lst) (insertR old new (cdr lst))))))

; 12.17 insertL
(define insertL
  (lambda (old new lst)
     (if (equal? (car lst) old)
        (cons new (cons old (cdr lst)))
        (cons (car lst) (insertL old new (cdr lst))))))

; 12.18 insertL
 (define subst
   (lambda (old new lst)
     (if (equal? (car lst) old)
         (cons new (cdr lst))
         (cons (car lst) (subst old new (cdr lst))))))

; 12.19 pick
 (define pick
   (lambda (lst n)
     (cond
       ((> (+ n 1) (list-length lst))
        '())
       ((= n 0)
        (car lst))
       (else
        (pick (cdr lst) (- n 1))))))

; 12.19 rempick
 (define rempick
   (lambda (lst n)
     (cond
       ((> (+ n 1) (list-length lst))
        '())
       ((= n 0)
        (cdr lst))
       (else
        (cons (car lst) (rempick (cdr lst) (- n 1)))))))

; Activation
(fact-list `(1 2 3 4))

(member? 1 `(1 6 3 9))
(member? 9 `(1 6 3 9))
(member? 4 `(1 6 3 9))

(even? 2001)
(even? 101)

(remove-first 4 `(1 2 3 4 4 5))
(remove 4 `(4 1 2 4 3 4 4 5))

(firsts `((1 2 3) (5 6) (8) (9 3 1)))
(firsts `())

(insertR 8 39 `(1 2 4 8 16 32))
(insertL 8 39 `(1 2 4 8 16 32))

(subst 8 39 `(1 2 4 8 16 32))

(list-length `(1 2 4 8 16 32))

(pick `(1 2 4 8 16 32) 0)
(pick `(1 2 4 8 16 32) 3)
(pick `(1 2 4 8 16 32) 5)
(pick `(1 2 4 8 16 32) 6)

(rempick `(1 2 4 8 16 32) 0) 
(rempick `(1 2 4 8 16 32) 3)
(rempick `(1 2 4 8 16 32) 9) 