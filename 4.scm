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

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else
        (cons (+ (car tup1)
                 (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(define >
  (lambda (m n)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else
        (> (sub1 m) (sub1 n))))))

(define <
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else
        (< (sub1 m) (sub1 n))))))

(define =
  (lambda (m n)
    (cond
      ((and (zero? n) (zero? m))
       #t)
      ((zero? n)
       #f)
      ((zero? m)
       #f)
      (else
        (= (sub1 m) (sub1 n))))))

(define =
  (lambda (m n)
    (cond
      ((< m n) #f)
      ((> m n) #f)
      (else #t))))

(define higher
  (lambda (m n)
    (cond
      ((< m n) n)
      ((> m n) m)
      ((= m n) m))))

(define exponent
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (exponent n (sub1 m)))))))

(define /
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (/ (- n m) m))))))

(define %
  (lambda (n m)
    (cond
      ((< n m) n)
      (else (/ (- n m) m)))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n) (cdr lat))
      (else (cons (car lat)
                  (rempick
                    (sub1 n)
                    (cdr lat)))))))




(puts '(
   '(3 6 9 11 4 1  1 1 )
   ; (pick 1 '(3 6 9 11 4 1  1 1 ) )
   ; (pick 2 '(3 6 9 11 4 1  1 1 ) )
   ; (pick 3 '(3 6 9 11 4 1  1 1 ) )
   ; (pick 4 '(3 6 9 11 4 1  1 1 ) )
   ; (/  8 3)
   ; (/  4009 289)
))

