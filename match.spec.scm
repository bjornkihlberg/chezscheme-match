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
    "Unexpected predicate pattern (?), expected (? predicate pattern) or (? pattern) in")
  (expand '(match 5 [(?) 4])))

(expect-error e
  (assert-with string=?
    (condition-message e)
    "Unexpected predicate pattern (? even? x y), expected (? predicate pattern) or (? pattern) in")
  (expand '(match 5 [(? even? x y) 4])))

(assert-with eq? 9
  (match 9 [(? x) x]))

(assert-with eq? (void)
  (match #f [(? x) x]))

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

(assert-with equal? '(3)
  (match '(3) [`,x x]))

(assert-with equal? 3
  (match '(huey dewey louie) [`,(-> length n) n]))

(assert-with equal? '(knatte fnatte . tjatte)
  (match '(knatte fnatte . tjatte) [`(,@x) x]))

(assert-with eq? 'hey
  (match 'hey [`(,@x) x]))

(assert-with eq? 'success
  (match '() [`() 'success]))

(assert-with eq? (void)
  (match '() [`x 'success]))

(assert-with eq? 'success
  (match 'x [`x 'success]))

(assert-with eq? 'success
  (match '42 [`42 'success]))

(assert-with eq? (void)
  (match '42 [`43 'success]))

(assert-with eq? '()
  (match '() [`(,@x) x]))

(assert-with eq? 'success
  (match '(huey dewey louie) [`(huey dewey louie) 'success]))

(assert-with eq? 'correct
  (match '(huey dewey)
    [`(huey dewey louie) 'wrong]
    [`(huey dewey) 'correct]))

(assert-with eq? 'dewey
  (match '(huey dewey louie) [`(huey ,x louie) x]))

(assert-with equal? '(louie)
  (match '(huey dewey louie) [`(huey ,_ ,@xs) xs]))

(assert-with eq? (void)
  (match '(1 2 3 4 5) [`(huey ,@(-> length xs)) xs]))

(assert-with eq? 4
  (match '(1 2 3 4 5) [`(1 ,@(-> length xs)) xs]))

(assert-with eq? 'success
  (match '#() [`#() 'success]))

(assert-with eq? (void)
  (match '#() [`#(5) 'success]))

(assert-with eq? (void)
  (match '#(4) [`#(5) 'success]))

(assert-with eq? (void)
  (match '#(4 2) [`#(5) 'success]))

(assert-with eq? 2
  (match '#(1 2 3) [`#(1 ,x 3) x]))

(assert-with eq? (+ 3 5)
  (match '#(1 #(2 3 4) 5) [`#(1 #(2 ,x 4) ,y) (+ x y)]))

(assert-with equal? '(5 6 7)
  (match '#(4 5 6 7) [`#(,_ ,@xs) xs]))

(assert-with equal? '(4 5 6 7)
  (match '#(4 5 6 7) [`#(,@xs) xs]))

(assert-with equal? '(7)
  (match '#(4 5 6 7) [`#(4 5 6 ,@xs) xs]))

(assert-with eq? '()
  (match '#(4 5 6 7) [`#(4 5 6 7 ,@xs) xs]))

(define t1 (current-time))
(display "All tests passed!\n")
(format #t "~s\n" (time-difference t1 t0))
