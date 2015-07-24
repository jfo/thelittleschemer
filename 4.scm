(load "123.scm")

(define add1
  (lambda (x)
    (+ x 1)))

(define sub1
  (lambda (x)
    (- x 1)))

(define +
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (+ n (sub1 m)))))))

(define -
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (- n (sub1 m)))))))

(define myaddtup
  ; succcccks
  (lambda (tup)
    (cond
      ((null? (cdr (cdr tup)))
       (+
         (car tup)
         (car (cdr tup))))
      (else
        (addtup
          (cons
            (+ (car tup)
               (car (cdr tup)))
            (cdr (cdr tup))))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))

(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else
        (+ n (* n (sub1 m)))))))

(puts '(

    (addtup '(1 2 3 4 5))
    (* 4000 89)

))
