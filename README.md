# dydx

A derivative calculator for Lisp forms, made in Racket.

Usage example:

```scheme
(derive 'x '(expt x 2))
```

This will calculate the derivative of x² in respect to x. The result
of the call is a Lisp form you can evaluate. (assuming you set `x` to
something meaningful beforehand)

The call above should return:

```scheme
(* 2 x)
```

For a list of supported functions (mostly trigonometric), look up the
value of `common-derivatives`. It is an alist-mapping of common
functions to their derivatives in respect to x.
