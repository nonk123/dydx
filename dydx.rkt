#lang racket

(define common-derivatives
  '([sin . (cos x)]
    [cos . (* (sin x) -1)]
    [log . (/ 1 x)]
    [tan . (/ 1 (expt (cos x) 2))]
    [cotan . (/ -1 (expt (sin x) 2))] ; not a real Scheme function
    [asin . (/ 1 (sqrt (- 1 (expt x 2))))]
    [acos . (/ -1 (sqrt (- 1 (expt x 2))))]
    [atan . (/ 1 (+ 1 (expt x 2)))]
    [acotan . (/ -1 (+ 1 (expt x 2)))] ; not real either
    [sinh . (cosh x)]
    [cosh . (sinh x)]
    [tanh . (/ 1 (expt (cosh x) 2))]
    [cotanh . (/ -1 (expt (sinh x) 2))])) ; no need to mention this is not real

(define (simplify expression)
  (letrec ([constant?
            (lambda (expression)
              (cond
               [(number? expression)
                #t]
               [(list? expression)
                (andmap constant? (cdr expression))]
               [else
                #f]))])
    (cond
     [(constant? expression)
      (eval expression)]
     [(list? expression)
      (let ([left (simplify (cadr expression))]
            [right (simplify (caddr expression))]
            [operator (car expression)])
        (set! expression (list operator left right))
        (case (car expression)
          [(+ -)
           (cond
            [(eqv? 0 left) right]
            [(eqv? 0 right) left]
            [else expression])]
          [(*)
           ;; Handle multiplication by zero and one.
           (cond
            [(or (eqv? 0 left) (eqv? 0 right)) 0]
            [(eqv? 1 left) right]
            [(eqv? 1 right) left]
            [else expression])]
          [(/)
           ;; Simplify divison by one. Throw an error on divison by zero.
           (cond
            [(eqv? 1 right) left]
            [(eqv? 0 right) (error "Cannot divide by zero")]
            [else expression])]
          [(expt)
           (cond
            ;; 1^x, 0^x, x^1, x^0.
            [(eqv? 1 left) 1]
            [(eqv? 0 left) 0]
            [(eqv? 1 right) left]
            [(eqv? 0 right) 1]
            [else expression])]
          [else expression]))]
     [else
      expression])))

(define (derive argument function)
  (letrec ([-derive (lambda (function) (derive argument function))]
           [argument? (lambda (expr)
                        (if (list? expr)
                            (and (not (null? expr))
                                 (or (argument? (car expr))
                                     (argument? (cdr expr))))
                            (equal? expr argument)))]
           [constant? (lambda (expr) (not (argument? expr)))]
           [replace (lambda (function new-argument)
                      (cond
                       [(null? function) '()]
                       [(eqv? function 'x) new-argument]
                       [(list? function)
                        (cons (replace (car function) new-argument)
                              (replace (cdr function) new-argument))]
                       [else function]))])
    (simplify
     (cond
      [(list? function)
       (let ([operator (car function)]
             [left (cadr function)]
             [right (or (and (not (null? (cddr function)))
                             (caddr function))
                        '())])
         (case operator
           ;; (f(x) +- g(x))' = f'(x) +- g'(x)
           [(+ -)
            (list operator (-derive left) (-derive right))]
           ;; (f(x)g(x))' = f'(x)g(x) + f(x)g'(x)
           [(*)
            `(+ (* ,(-derive left) ,right) (* ,(-derive right) ,left))]
           ;; (f(x) / g(x))' = (f'(x)g(x) - f(x)g'(x)) / (g(x))^2
           [(/)
            `(/ (- (* ,(-derive left) ,right)
                   (* ,(-derive right) ,left))
                (* ,right ,right))]
           [(expt)
            (cond
             ;; (c^c)' = 0
             [(and (constant? left) (constant? right)) 0]
             ;; (c^x)' = c^x * ln(c)
             [(and (constant? left) (argument? right))
              `(* ,(-derive right) (* ,function (log ,left)))]
             ;; (x^c)' = cx^(c - 1)
             [(and (argument? left) (constant? right))
              `(* ,(-derive left) (* ,right (expt ,left (- ,right 1))))]
             ;; TODO: implement (x^x)'.
             [else (error "x^x is currently not implemented")])]
           [else
            (if (assq operator common-derivatives)
                (let ([function (cdr (assq operator common-derivatives))])
                  `(* ,(-derive left) ,(replace function left)))
                (error "Unknown function:" operator))]))]
      [(symbol? function)
       (if (argument? function) 1 function)]
      [(constant? function) 0]))))
