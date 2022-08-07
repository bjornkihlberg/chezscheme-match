(library (match)
  (export if-match match match-lambda)
  (import (chezscheme))
  (define-syntax if-match
    (syntax-rules (unquote-splicing quasiquote unquote)
      [(_ value `,x bodys bodyf) ((lambda (x) bodys) value)]
      [(_ value `,x t bodys bodyf) ((lambda (x) (if t bodys bodyf)) value)]
      [(_ value `(,@x) bodys bodyf)
       (if (list? value) ((lambda (x) bodys) value) bodyf)]
      [(_ value `(,@x) t bodys bodyf)
       (if (list? value) ((lambda (x) (if t bodys bodyf)) value) bodyf)]
      [(_ value `(x) bodys bodyf)
       (if (and (pair? value) (null? (cdr value)))
           (if-match (car value) `x bodys bodyf)
           bodyf)]
      [(_ value `(x) t bodys bodyf)
       (if (and (pair? value) (null? (cdr value)))
           (if-match (car value) `x t bodys bodyf)
           bodyf)]
      [(_ value `(x xs ...) bodys bodyf)
       (if (and (pair? value) (not (null? (cdr value))))
           (if-match
             (car value)
             `x
             (if-match (cdr value) `(xs ...) bodys bodyf)
             bodyf)
           bodyf)]
      [(_ value `(x xs ...) t bodys bodyf)
       (if (and (pair? value) (not (null? (cdr value))))
           (if-match
             (car value)
             `x
             (if-match (cdr value) `(xs ...) t bodys bodyf)
             bodyf)
           bodyf)]
      [(_ value `x bodys bodyf) (if (equal? value 'x) bodys bodyf)]
      [(_ value `x t bodys bodyf) (if (and (equal? value 'x) t) bodys bodyf)]))
  (define-syntax match
    (syntax-rules (else)
      [(_ value) (void)]
      [(_ value (else body)) body]
      [(_ value (pattern body) rest ...)
       (if-match value pattern body (match value rest ...))]
      [(_ value (pattern test body) rest ...)
       (if-match value pattern test body (match value rest ...))]))
  (define-syntax match-lambda
    (syntax-rules () [(_ patterns ...) (lambda (x) (match x patterns ...))])))
