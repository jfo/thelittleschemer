(load "7.scm")


(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l)
                  (rember-f test? a (cdr l)))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l)
                    (rember-f test? a (cdr l))))))))

(define rember-eq?  (rember-f eq?))
(define rember-eqan?  (rember-f eqan?))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

; (define insertL-f
;   (lambda (test?)
;     (lambda (new old lat)
;       (cond
;         ((null? lat) '())
;         ((test? old (car lat))
;          (cons new (cons old (cdr lat))))
;         (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))

; (define insertL-f
;   (lambda (test?)
;     (lambda (new old lat)
;       (cond
;         ((null? lat) '())
;         ((test? old (car lat))
;          (cons old (cons new (cdr lat))))
;         (else (cons (car lat) ((insertL-f test?) new old (cdr lat))))))))


(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define sub
  (lambda (new old l)
    (cons new l)))

(define insert-g
  (lambda (dir)
    (lambda (test?)
      (lambda (new old lat)
        (cond
          ((null? lat) '())
          ((test? old (car lat))
           (dir new old (cdr lat)))
          (else (cons (car lat) ((insert-g test?) new old (cdr lat)))))))))

(define insertL-f
  (lambda (new old lat)
    (((insert-g seqL) eqan?) new old lat)))


(define insertR-f
  (lambda (new old lat)
    (((insert-g seqR) eqan?) new old lat)))
(define insertR-f
  (lambda (new old lat)
    (((insert-g seqR) eqan?) new old lat)))

(define subst
  (lambda (new old lat)
    (((insert-g sub) eqan?) new old lat)))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x '+) +)
      ((eq? x '*) *)
      (else exponent))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function
           (operator nexp))
         (1st-sub-exp nexp)
         (2nd-sub-exp nexp))))))





(puts '(

        (value (exponent 3 9))

        ; (insertL-f 'butts 'farts  '(farts (farts)))
        ; (insertR-f 'butts 'farts  '(farts (farts)))
        ; (subst 'butts 'farts  '(farts (farts) farts))

))
