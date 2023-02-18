(print-gensym 'pretty)

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
      (let ([error (gensym "error")] [success (gensym "success")])
        (assert-with symbol=? error (guard (e [else expectation error]) expression success)))]))

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

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected clause 4, expected [pattern expression ...] in")
  (expand '(match 5 4)))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected pattern () in")
  (expand '(match 5 [() 4])))

(assert-with eq? 5
  (match 4 [x (add1 x)]))

(assert-with eq? 'success
  (match 4 [4 'success]))

(assert-with eq? 'success
  (match "huey" ["huey" 'success]))

(assert-with eq? 'success
  (match '(1 2 3) ['(1 2 3) 'success]))

(assert-with eq? 'success
  (match '() ['() 'success]))

(assert-with eq? (void)
  (match 4 [5 'success]))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected named pattern (&), expected (& pattern pattern pattern ...) in")
  (expand '(match 5 [(&) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected named pattern (& x), expected (& pattern pattern pattern ...) in")
  (expand '(match 5 [(& x) 4])))

(assert-with eq? 18
  (match 6 [(& x y z) (+ x y z)]))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected view pattern (->), expected (-> procedure pattern) in")
  (expand '(match 5 [(->) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected view pattern (-> add1), expected (-> procedure pattern) in")
  (expand '(match 5 [(-> add1) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected view pattern (-> add1 x y), expected (-> procedure pattern) in")
  (expand '(match 5 [(-> add1 x y) 4])))

(assert-with eq? 8
  (match 7 [(-> add1 x) x]))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected predicate pattern (?), expected (? predicate pattern) in")
  (expand '(match 5 [(?) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected predicate pattern (? odd?), expected (? predicate pattern) in")
  (expand '(match 5 [(? odd?) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected predicate pattern (? even? x y), expected (? predicate pattern) in")
  (expand '(match 5 [(? even? x y) 4])))

(assert-with eq? 9
  (match 9 [(? odd? x) x]))

(assert-with eq? (void)
  (match 11 [(? even? x) x]))

(assert-with eq? 13
  (match 12 [x (void) (add1 x)]))

(assert-with eq? (void)
  (match 3 [x (? #f) x]))

(assert-with eq? 15
  (match 15 [x (? #t) x]))

(expect-error e (void) (expand '(match 17 [_ _])))

(assert-with eq? 'success
  (match 16 [x (? #f) x]
            [_ 'success]))

(assert-with eq? (void)
  (match 6 [(& x (? odd? y) z) (+ x y z)]))

(define t1 (current-time))
(display "All tests passed!\n")
(format #t "~s\n" (time-difference t1 t0))
