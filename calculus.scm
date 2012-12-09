#lang racket


"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"
;Method I:
(define (bitfunc x)
    (- (+ (* (* x x) (* x x)) (* x x)) 14))
;Method II:
(define (bitfunc x)
    (- (+ (expt x 4) (expt x 2)) 14))


"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2) 
    (abs (* (- x1 x2) (bitfunc x1))))


"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-iter num-steps x1 x2)
    (if (= x1 x2) 
        0
        (+ 
            (bitfunc-rect x1 (+ x1 (/ (- x2 x1) num-steps))) 
            (bitfunc-integral-iter (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
        )
    )
; I can not separate it into two function, so it is just one function here. But it works as well.

"Problem 4: Integrating any function"

(define (integral func num-steps x1 x2)  ; here the func need to be quoted to pass into the function
    (begin
        (define (f x) func)
        (define (inner function num-steps x1 x2)
            (if (= x1 x2) 
               0
               (+ 
                    (abs (* (- x1 (+ x1 (/ (- x2 x1) num-steps))) (function x1))) 
                    (integral (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
               )
            )
    (inner f num-steps x1 x2)
    )
)

"Problem 5: Area of a unit circle"

(define (approx-pi num-steps)
    (* 4 (integral '(sqrt (- 1 (expt x 2))) num-steps 0 1)))

"Problem 6: Integrating with pieces of any shape"

(define (rectangle func x1 x2) 
    (abs (* (- x1 x2) (func x1))))

(define (rectangle-iter func num-steps x1 x2)
    (if (= x1 x2) 
        0
        (+ 
            (rectangle func x1 (+ x1 (/ (- x2 x1) num-steps))) 
            (rectangle-iter func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
        )
    )

(define (trapezoid func x1 x2)
    (abs (/ (* (- x1 x2) (+ (func x1) (func x2)) 2))

(define (trapezoid-iter func num-steps x1 x2)
    (if (= x1 x2) 
        0
        (+ 
            (trapezoid func x1 (+ x1 (/ (- x2 x1) num-steps))) 
            (trapezoid-iter func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))
        )
    )

(define (integral-with piece func num-steps x1 x2)
    (if (piece = rectangle)
        (rectangle-iter func num-steps x1 x2)
        (trapezoid-iter func num-steps x1 x2)
        ))

"Problem 7: Better approximation of pi"

(define function (sqrt (- 1 (expt x 2))))

(define (better-pi num-steps)
    (* 4 (trapezoid-iter function num-steps 0 1)))


"Part 2: Symbolic differentiation"

(define (deriv-constant constant wrt)
    0)


"Problem 8: Derivative of a variable"

(define (deriv-variable var wrt)
       (if (and (symbol? var) (symbol? wrt) (eq? var wrt)) 
         1 0))

"Problem 9: Calling the right function"

(define (derivative expr wrt)
    (cond
        ((number? expr) 0)
        ((deriv-variable expr wrt) 1) 
        (else (error "Don't know how to differentiate" expr))))


"Problem 10: Derivative of a sum"

(define (sum? expr)
       (and (pair? expr) (eq? (car expr) '+)))

(define (deriv-sum expr wrt)
    (cond
        ((number? expr) 0)
        ((deriv-variable expr wrt) 1) 
        ((sum? expr)
            (list '+ 
                (deriv (cadr expr) wrt)
                (deriv (caddr expr) wrt)))
        (else (error "Don't know how to differentiate" expr))))


"Problem 11: Derivative of a product"

(define (product? expr)
       (and (pair? expr) (eq? (car expr) '*)))

(define (deriv-product expr wrt)
    (cond
        ((number? expr) 0)
        ((symbol? expr)
            ((deriv-variable expr wrt) 1 0))
        ((sum? expr)
            (list '+ 
                (deriv (cadr expr) wrt)
                (deriv (caddr expr) wrt)))
        ((product? expr)
             (list '+ 
               (list '* (cadr expr)
                        (deriv (caddr expr) wrt))
               (list '* (deriv (cadr expr) wrt)
                        (caddr expr))))
        (else (error "Don't know how to differentiate" expr))))


"Problem 12: Additional testing"

(define (divide? expr)
       (and (pair? expr) (eq? (car expr) '/)))

(define (deriv-Add expr wrt)
    (cond
        ((number? expr) 0)
        ((symbol? expr)
            ((deriv-variable expr wrt) 1 0))
        ((sum? expr)
            (list '+ 
                (deriv (cadr expr) wrt)
                (deriv (caddr expr) wrt)))
        ((product? expr)
            (list '+ 
               (list '* (cadr expr)
                        (deriv (caddr expr) wrt))
               (list '* (deriv (cadr expr) wrt)
                        (caddr expr))))
        ((divide? expr)
            (list (list '-                       
                    (list '* (deriv (cadr expr) wrt)
                            (caddr expr))
                    (list '* (cadr expr)
                            (deriv (caddr expr) wrt)))
                    '/ 
                    (list 'expt (caddr expr) 2)
            ))
        (else (error "Don't know how to differentiate" expr))))


