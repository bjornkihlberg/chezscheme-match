(define-syntax assert-with
  (syntax-rules ()
    [(_ comparison a b)
      (let ([actual a] [expected b])
        (guard
          (e [else (format #t "~a, namely the expression: (~a ~s ~s)\n" (condition-message e) 'comparison actual expected)
                   (exit 1)])
          (assert (comparison actual expected))))]))

(display "Running tests...\n")
(define t0 (current-time))

(module (match)
  (include "match.impl.scm"))

(assert-with eq? 5 (match 5))

(let ()
  (import (match))
  (assert-with equal? (library-exports '(match)) '(match)))

(define t1 (current-time))
(display "All tests passed!\n")
(format #t "~s\n" (time-difference t1 t0))
