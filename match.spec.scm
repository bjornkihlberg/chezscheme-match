(define-syntax assert-with
  (syntax-rules ()
    [(_ comparison a b)
      (let ([actual a] [expected b])
        (guard
          (e [else (format #t "~a, namely the expression: (~a ~s ~s)\n" (condition-message e) 'comparison actual expected)
                   (exit 1)])
          (assert (comparison actual expected))))]))

(define-syntax expect-error
  (syntax-rules ()
    [(_ e expectation expression)
      (let ([error (gensym "error")])
        (assert-with symbol=? error (guard (e [else expectation error]) expression)))]))

(display "Running tests...\n")
(define t0 (current-time))

(module (match)
  (include "match.impl.scm"))

(let ()
  (import (match))
  (assert-with equal? (library-exports '(match)) '(match)))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Missing value, expected (match value clause ...) in")
  (expand '(match)))

(assert-with eq? (void) (match 5))

(define t1 (current-time))
(display "All tests passed!\n")
(format #t "~s\n" (time-difference t1 t0))
