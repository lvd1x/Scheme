(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (reverse lst)
  (define (rev-tail olds news)
    (if (null? olds) news
      (rev-tail (cdr olds) (cons (car olds) news))
      ))
  (rev-tail lst nil)
  )

(define (map proc items)
  (if (null? items) nil
    (cons (proc (car items)) (map proc (cdr items)))
    )
  )

(define (cons-all first rests)
  (map (lambda (rest) (cons first rest)) rests)
  )

(define (zip pairs)
  (list (map car pairs) (map cadr pairs))
  )

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (enum_iter u v i)
    (if (null? u) v
      (enum_iter (cdr u) (cons (cons i (cons (car u) nil)) v) (+ i 1))
    ))
  (reverse (enum_iter s '() 0))
  )
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (define correct-list nil)

  (define (make-change lst amount denoms)
    (cond ((= amount 0) (cons-all correct-list lst))
      ((null? denoms) nil)
      ((< amount 0) nil)  
      (else (cons-all (make-change (append lst (list (car denoms))) (- amount (car denoms)) denoms) (make-change lst amount (cdr denoms)))) 
        )
      )

    (define (fix lst)
      (cond ((null? lst) nil)
        ((and (list? (car lst)) (list? (caar lst))) (append (fix (car lst)) (fix (cdr lst))))
        (else (cons (car lst) (fix (cdr lst))))
        )
      )

    (fix (make-change nil total denoms))
    )
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (append (list form params) (map analyze body))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (define zipped-values (zip values))
           (define param (car zipped-values))
           (define param-values (map analyze (cadr zipped-values)))
           (define fn (append (list 'lambda param) (map analyze body)))
           (cons fn param-values)
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr) 
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21

