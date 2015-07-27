(load "6.scm")


(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
       (else
         (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons
         (car lat)
         (makeset
           (multirember
             (car lat)
             (cdr lat))))))))

(define eqset?
  (lambda (set1 set2)
    (and
      (subset? set2 set1)
      (subset? set1 set2))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else
        (and (member? (car set1) set2)
             (subset? (cdr set1) set2))))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else
        (or (member? (car set1) set2)
            (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

; there is never any typechecking, this is awful
(define union
 (lambda (set1 set2)
   (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

; guaranteed to return a set of union between two lats that are sets or not
; or whatever
;
(define myunion
  (lambda (set1 set2)
    (cond
      ((null? set1) (makeset set2))
      (else (union
              (cdr set1)
              (cons
                (car set1)
                set2))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
        (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))

(define myintersectall
  (lambda (l-set)
    (cond
      ((eqan? (length l-set) 2)
       (intersect (car l-set)
                  (car (cdr l-set))))
      (else
        (intersectall
          (cons
            (intersect (car l-set)
                       (car (cdr l-set)))
            (cdr (cdr l-set))))))))

; theirs: much better, not as much contortion
(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else
        (intersect (car l-set)
                   (intersectall (cdr l-set)))))))

(define apair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda ( p )
    (car (cdr p))))
(define third
  (lambda (p)
    (car (cdr (cdr p)))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define revpair
  (lambda (pair)
    (build
      (second pair)
      (first pair))))
(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else
        (cons
          (revpair (car rel))
          (revrel (cdr rel)))))))


(puts '(
        ))
