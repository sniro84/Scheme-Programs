; Course: EOPL, The Open University
; Maman: 11
; Name: Snir Holland
; ID: 039998943

; Answer 1
; --------
; my-flatten: returns a flattened list
(define my_flatten
  (lambda (lst)
    (define my_concat ; helper method to concat two lists
      (lambda (lst1 lst2)
        (if (null? lst1)
            lst2
            (cons (car lst1) (my_concat (cdr lst1) lst2)))))
    (cond ((null? lst) '())  
          ((not (list? lst))
             (list lst))
          (else
             (my_concat (my_flatten (car lst)) (my_flatten (cdr lst)))))))

; unit tests for my_flatten
(define (test1-my_flatten)
  (if (equal? (my_flatten '((1 2) ((3)) (4 5 6))) '(1 2 3 4 5 6))
     "my_flatten: Test case 1 Success"
     "my_flatten: Test case 1 Failure"))
(test1-my_flatten)

(define (test2-my_flatten)
  (if (equal? (my_flatten '()) '())
     "my_flatten: Test case 2 Success"
     "my_flatten: Test case 2 Failure"))
(test2-my_flatten)

(define (test3-my_flatten)
  (if (equal? (my_flatten '(1 2 3)) '(1 2 3))
     "my_flatten: Test case 3 Success"
     "my_flatten: Test case 3 Failure"))
(test3-my_flatten)

(define (test4-my_flatten)
  (if (equal? (my_flatten '(1 (2 (3 (4))))) '(1 2 3 4))
     "my_flatten: Test case 4 Success"
     "my_flatten: Test case 4 Failure"))
(test4-my_flatten)

(define (test5-my_flatten)
  (if (equal? (my_flatten '(1 . (2 . (3 . (4))))) '(1 2 3 4))
     "my_flatten: Test case 5 Success"
     "my_flatten: Test case 5 Failure"))
(test5-my_flatten)

