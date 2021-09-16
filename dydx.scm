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
    [cotanh . (/ -1 (expt (sinh x) 2))]))

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
     [(constant? function) 0])))
