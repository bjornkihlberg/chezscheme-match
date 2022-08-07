# chezscheme-match

Pattern matching in Chez Scheme

---

## Quickstart

I'm assuming you know what pattern matching is and have experience with it from other programming languages.

There are three macros for pattern matching; `if-match`, `match` and `match-lambda`. You only need `if-match` technically. `match` and `match-lambda` are convenience macros but have the same capabilites as `if-match`. Each pattern is followed by an optional guard clause. The patterns are determined at compile time. Patterns use quasiquote syntax to mirror construction of structures.

- `(if-match value pattern guard on-match on-mismatch)`

  ```scheme
  (define my-value '(huey (dewey) louie))

  (if-match my-value `(huey (,x) ,y) x "no match!") ; dewey

  (if-match my-value `(huey (,x) ,y) (eq? x y) x "no match!") ; "no match!"
  ```

- `(match value (pattern guard on-match) ...)`

  ```scheme
  (define my-value '(huey (dewey) louie))

  (match my-value
    [`(,x (,y) louie) (eq? x y) y]
    [`(,x (,y) not-louie) y]
    [`(,x (,y) ,z) z]
    [else "no match!"]) ; 'louie
  ```

- `(match-lambda (pattern guard on-match) ...)`

  ```scheme
  (define my-value '(huey (dewey) louie))

  (define my-procedure
    (match-lambda [`(,x (,y) louie) (eq? x y) y]
                  [`(,x (,y) not-louie) y]
                  [`(,x (,y) ,z) z]
                  [else "no match!"]))

  (my-procedure my-value) ; 'louie
  ```
