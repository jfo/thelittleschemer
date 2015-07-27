(load "5.scm")

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) '+)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) '*)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) 'exponent)
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
             (numbered?
               (car (cdr (cdr aexp)))))))))

(define 1st-sub-exp
  (lambda (aexp)
    (value (car aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (value (car (cdr (cdr aexp))))))

(define operator
  (lambda (aexp)
    (car (cdr aexp))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (+ (1st-sub-exp nexp)
          (2nd-sub-exp nexp)))
      ((eq? (operator nexp) '*)
       (* (1st-sub-exp nexp)
          (2nd-sub-exp nexp)))
      ((eq? (operator nexp) 'exponent)
       (exponent (1st-sub-exp nexp)
                 (2nd-sub-exp nexp))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define pluz
  (lambda (n m)
  (cond
    ((sero? m) n)
    (else (edd1 (pluz n (zub1 m)))))))

(define minuz
  (lambda (n m)
  (cond
    ((sero? m) n)
    (else (edd1 (pluz n (zub1 m)))))))

(puts '(


))
