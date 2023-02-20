(define-syntax (match code)
  (define macro-name (car (syntax->datum code)))
  (define macro-args (cdr (syntax->datum code)))

  (define (match-errorf . args)
    (syntax-violation macro-name (string-append (apply format args) " in") code))

  ; Check if pattern is variable binding
  (define (pattern-variable? pattern) (symbol? pattern))

  ; Check if pattern is literal comparison
  (define (pattern-literal? pattern)
    (or (and (atom? pattern) (not (null? pattern)))
        (and (pair? pattern) (eq? (car pattern) 'quote))))

  ; Check if pattern is wildcard pattern
  (define (pattern-wildcard? pattern) (eq? pattern '_))

  (define (wrap-mismatch-in-thunk on-mismatch mini-macro)
    (syntax-case on-mismatch ()
      [(expression) (symbol? #'expression)
        (mini-macro on-mismatch)]

      [_
        (let ([on-mismatch-thunk (gensym "on-mismatch-thunk")])
          `(let ([,on-mismatch-thunk (lambda () ,on-mismatch)]) 
              ,(mini-macro (list on-mismatch-thunk))))]))

  (define (match-quasiquoted-vector match-value pattern on-match on-mismatch)
    (let loop ([offset 0] [pattern pattern])
      (syntax-case pattern (unquote unquote-splicing)
        [()
          `(if (= ,offset (vector-length ,match-value)) ,on-match ,on-mismatch)]

        [(,@pattern)
          (let ([new-match-value (gensym "match-value-a")])
            `(let loop ([i (sub1 (vector-length ,match-value))] [,new-match-value '()])
              (if (>= i ,offset)
                  (loop (sub1 i) (cons (vector-ref ,match-value i) ,new-match-value))
                  ,(match-clause new-match-value #'pattern on-match on-mismatch))))]

        [(pattern . patterns)
          `(if (> (vector-length ,match-value) ,offset)
              ,(match-quasiquotation `(vector-ref ,match-value ,offset) #'pattern
                (loop (add1 offset) #'patterns)
                on-mismatch)
              ,on-mismatch)])))

  (define (match-quasiquotation match-value pattern on-match on-mismatch)
    (syntax-case pattern (unquote unquote-splicing)
      [()
        `(if (null? ,match-value) ,on-match ,on-mismatch)]

      [#(pattern ...)
        (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
          `(if (vector? ,match-value)
            ,(match-quasiquoted-vector match-value #'(pattern ...) on-match on-mismatch)
            ,on-mismatch)))]

      [literal (atom? #'literal)
        `(if (equal? ',#'literal ,match-value) ,on-match ,on-mismatch)]

      [,x
        (match-clause match-value #'x on-match on-mismatch)]

      [(,@x)
        (match-clause match-value #'x on-match on-mismatch)]

      [(x . xs)
        (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
          (let ([new-match-value (gensym "match-value-b")])
            `(if (pair? ,match-value)
              ,(match-quasiquotation `(car ,match-value) #'x
                `(let ([,new-match-value (cdr ,match-value)])
                  ,(match-quasiquotation new-match-value #'xs on-match on-mismatch))
                on-mismatch)
              ,on-mismatch))))]))

  (define (match-clause match-value pattern on-match on-mismatch)
    (syntax-case pattern (& ? -> quasiquote)
      [(& . named-pattern-args*)
        (syntax-case #'named-pattern-args* ()
          [(pattern0 pattern1 pattern* ...)
            (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
              (fold-right (lambda (pattern on-match)
                  (match-clause match-value pattern on-match on-mismatch))
                on-match #'(pattern0 pattern1 pattern* ...))))]

          [unknown-pattern-args
            (match-errorf "Unexpected named pattern ~s, expected (& pattern pattern pattern ...)"
                          (cons '& #'unknown-pattern-args))])]

      [(? . predicate-pattern-args*)
        (syntax-case #'predicate-pattern-args* ()
          [(pattern)
            (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
              `(if ,match-value
                ,(match-clause match-value #'pattern on-match on-mismatch)
                ,on-mismatch)))]

          [(predicate pattern)
            (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
              `(if (,#'predicate ,match-value)
                ,(match-clause match-value #'pattern on-match on-mismatch)
                ,on-mismatch)))]

          [unknown-pattern-args
            (match-errorf "Unexpected predicate pattern ~s, expected (? predicate pattern) or (? pattern)"
                          (cons '? #'unknown-pattern-args))])]

      [(-> . view-pattern-args*)
        (syntax-case #'view-pattern-args* ()
          [(procedure pattern)
            (match-clause `(,#'procedure ,match-value) #'pattern on-match on-mismatch)]

          [unknown-pattern-args
            (match-errorf "Unexpected view pattern ~s, expected (-> procedure pattern)"
                          (cons '-> #'unknown-pattern-args))])]

      [(quasiquote pattern)
        (match-quasiquotation match-value #'pattern on-match on-mismatch)]

      [wildcard (pattern-wildcard? #'wildcard)
        on-match]

      [variable (pattern-variable? #'variable)
        `(let ([,#'variable ,match-value]) ,on-match)]

      [literal (pattern-literal? #'literal)
        `(if (equal? ,match-value ,#'literal) ,on-match ,on-mismatch)]

      [unknown-pattern
        (match-errorf "Unexpected pattern ~s" #'unknown-pattern)]))

  (define (match-clauses macro-args)
    (syntax-case macro-args ()
      [(_)
        '(void)]

      [(match-value clause . clause*)
        (syntax-case #'clause (?)
          [(pattern (? predicate) on-match . on-match*)
            (let ([k (if (null? #'on-match*) #'on-match `(begin ,#'on-match ,@#'on-match*))]
                  [on-mismatch-thunk (gensym "on-mismatch-thunk-f")])
              `(let ([,on-mismatch-thunk (lambda () ,(match-clauses (cons #'match-value #'clause*)))])
                ,(match-clause #'match-value #'pattern `(if ,#'predicate ,k (,on-mismatch-thunk)) `(,on-mismatch-thunk))))]

          [(pattern on-match . on-match*)
            (let ([k (if (null? #'on-match*) #'on-match `(begin ,#'on-match ,@#'on-match*))])
              (match-clause #'match-value #'pattern k (match-clauses (cons #'match-value #'clause*))))]

          [unknown-clause
            (match-errorf "Unexpected clause ~s, expected [pattern expression ...]" #'unknown-clause)])]))

  (define (match-wrap-value macro-args)
    (syntax-case macro-args ()
      [()
        (match-errorf "Missing value, expected (match value clause ...)")]

      [(_)
        '(void)]

      [(macro-arg . macro-args)
        (let ([match-value (gensym "match-value-c")])
          `(let ([,match-value ,#'macro-arg]) ,(match-clauses (cons match-value #'macro-args))))]))

  (datum->syntax #'code (match-wrap-value macro-args)))
