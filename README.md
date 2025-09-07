# chezscheme-match

Pattern matching in Chez Scheme

---

# library `(match)`

## syntax `match`

### Examples

#### Wildcard matching

```scheme
(match (list 1 2 3)
  [_ 123])
; 123
```

#### Pair deconstruction

```scheme
(match '(4 . 5)
  [`(,x . ,y) (+ x y)])
; 9
```

#### List deconstruction

```scheme
(match (list 1 2 3)
  [`(,x ,@xs) x])
; 1
```

```scheme
(match (list 1 2 3)
  [`(,x ,@xs) xs])
; (2 3)
```

```scheme
(match (list 1 2 3)
  [`(,x ,@xs) xs])
; (2 3)
```

#### View patterns

```scheme
(match 7
  [(x (add1 x) y) (cons x y)])
; (7 . 8)
```

```scheme
(match (list 1 2 3 4)
  [`(2 ,@_)                  'huey]
  [(x (even? (length x)) #t) 'dewey]
  [`(,_ ,(x (even? x) #t))   'louie])
; dewey
```

> 💡 View patterns can be used to deconstruct types that are not natively supported like record types and hash tables.
