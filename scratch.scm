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

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) lat)
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(display
  (insertR "topping" "fudge" '("ice" "cream" "with" "fudge" "for" "dessert"))
)

; (eval
;   (cons
;     (car (cdr '("" display)))
;     '("Hello World")))
