(load "123.scm")

(define (thing x) (+ x x))

; (define (derp) "fart")
(define (fart) "this is weird")
(define derp fart)

(define thing
  (lambda (x)
    (+ x x)))

(define thingy
  (lambda (x)
    (cond
      ((= x 0)))))


(puts '(

    (thingy 2)
    (thingy 0)

))
