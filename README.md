# chezscheme-match

Pattern matching in Chez Scheme

## Usage

```scheme
(import (match))
```

### Literal patterns

```scheme
(match 1337
  [5    'huey]
  [1337 'dewey]
  [x    'louie])
```
```
dewey
```

### Quasiquotation patterns

```scheme
(match '(1 2 3 4)
  [`(,x ,_ ,@xs) xs]
  [_             'boo])
```
```
(3 4)
```

### Guard patterns

```scheme
(match 1
  [(? even? x)    (sub1 x)]
  [(? integer? x) (add1 x)]
  [_              'boo])
```
```
2
```

### View patterns

```scheme
(match 1
  [(-> add1 2)    'yay]
  [_              'nay])
```
```
yay
```

### Dupe patterns

```scheme
(match 5
  [(@ 5 x)    (* 2 x)]
  [_          'nope])
```
```
10
```
