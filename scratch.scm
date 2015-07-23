(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? a (car lat)) #t)
      (else (member? a (cdr lat))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))

(define remberall
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((eq? a (car lat)) (rember a (cdr lat)))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(display
  (firsts '((1 2 3 4 5)
            (5 2 3 4 5)
            (4 2 3 4 5)
            (3 2 3 4 5)))
)

; (eval
;   (cons
;     (car (cdr '("" display)))
;     '("Hello World")))
