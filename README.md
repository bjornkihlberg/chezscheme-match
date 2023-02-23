# chezscheme-match

Pattern matching in Chez Scheme

---

## Quickstart

### Pattern match syntax

```scheme
(match value clause*)
```

Where `value` is an eagerly evaluated expression and clause is described with the following grammar:

```
<clause>
  ::= (pattern guard-clause expression*)
    | (pattern expression*)

<guard-clause>
  ::= (? expression)

<pattern>
  ::= symbol                        ; Variable binding, eg x, y or z
    | literal                       ; Literal, eg "hello", 5 or '(huey dewey louie)
    | (& pattern pattern pattern*)  ; Multiple patterns on same value
    | (-> expression pattern)       ; Apply value to <expression> and match <pattern> on result
                                    ; <expression> must evaluate procedure of arity 1
    | (? expression pattern)        ; Apply value to <expression> and match <pattern> on value if result is not #f
                                    ; <expression> must evaluate procedure of arity 1
    | (? pattern)                   ; Match <pattern> on value if value is not #f
    | `quasi-quotation-pattern      ; Quasiquotation pattern matching on lists and vectors
    | _                             ; Wildcard pattern, matches on anything and binds nothing

<quasi-quotation-pattern>
  ::= ,pattern                      ; unquote value and match on <pattern>
    | (quasi-quotation-pattern*)    ; Match when value is list
    | (,@pattern)                   ; unquote splice value and match on <pattern>
    | #(quasi-quotation-pattern*)   ; Match when value is vector
```

Where `expression` can be any **Chez Scheme** expression.

### Examples

```scheme
(import (match))
```

```scheme
> (match 5
    ; Match on anything and bind it to x:
    [x (add1 x)])
6
```

```scheme
> (match "hello"
    ; Match on string if its length is an even number:
    [x (? (even? (string-length x))) x]
    ; Match on anything:
    [_ 'boo])
'boo
```

```scheme
> (match '(1 2 3 4 5 6)
    ; Bind list to ls and match its length with literal 5:
    [(& ls (-> length 5)) ls]
    [_ 'boo])
'boo
```

```scheme
> (match '(1 2 3 4 5 6)
    ; Bind list to ls and match if its length is even:
    [(& ls (-> length (? even? _))) (cdr ls)]
    [_ 'boo])
'(2 3 4 5 6)
```

```scheme
> (match '(1 2 #(3 4 5) 6 7 8)
    ; Match if third value is vector and its first value equals its length:
    [`(,_ ,_ ,(& (? vector? _) (-> vector-length n) `#(,x 4 5)) ,@ls) (? (= n x)) ls]
    [_ 'boo])
'(6 7 8)
```

The syntax should be generic enough such that you can implement more specific match syntax for custom data types not natively supported by `match`:

```scheme
> (define-record-type my-record (fields x y))
> (match `(1 ,(make-my-record 2 3) 4)
    [`(1 ,(& (? my-record? x) (-> my-record-x 5)) 4) 'nay]
    [`(1 ,(& (? my-record? x) (-> my-record-x 2)) 4) 'yay]
    [_ 'boo])
'yay
```

*More examples can be found in **./match.spec.scm***
