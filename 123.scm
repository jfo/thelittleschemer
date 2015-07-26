(define put
  (lambda (l)
    (display (eval l))
    (newline)))

(define puts
  (lambda (l)
    (map put (reverse l))))


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
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat)
              (rember a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) lat)
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons (car lat) (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)
       '())
      ((or (equal? o1 (car lat))
           (equal? o2 (car lat)))
       (cons new (cdr lat)))
      (else
        (cons (car lat) (subst2 new o1 o2(cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat)
       '())
      ((equal? old (car lat))
       (cons (car lat)
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else
        (cons (car lat)
              (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat))
       (cons new
             (cons (car lat)
                   (multiinsertL new old (cdr lat)))))
      (else
        (cons (car lat)
              (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
