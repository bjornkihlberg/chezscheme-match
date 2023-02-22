# chezscheme-match

Pattern matching in Chez Scheme

---

> 🪳 Bug in `match` macro where keywords are not recognized. Example:
> ```scheme
> > (match 3 [x (? (odd? x)) 'success])
> Exception: variable ? is not bound
> ```
> Will be fixed shortly!

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
