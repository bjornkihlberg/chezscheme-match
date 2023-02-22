(define-syntax (match code)
  (define macro-name (car (syntax->datum code)))
  (define macro-args (cdr (syntax->list code)))

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
        #`(let ([on-mismatch-thunk (lambda () #,on-mismatch)]) 
            #,(mini-macro #'(on-mismatch-thunk)))]))

  (define (match-quasiquoted-vector match-value pattern on-match on-mismatch)
    (let loop ([offset 0] [pattern pattern])
      (syntax-case pattern (unquote unquote-splicing)
        [()
          #`(if (= #,offset (vector-length #,match-value)) #,on-match #,on-mismatch)]

        [(,@pattern)
          #`(let loop ([i (sub1 (vector-length #,match-value))] [match-value-a '()])
              (if (>= i #,offset)
                (loop (sub1 i) (cons (vector-ref #,match-value i) match-value-a))
                #,(match-clause #'match-value-a #'pattern on-match on-mismatch)))]

        [(pattern . patterns)
          #`(if (> (vector-length #,match-value) #,offset)
              #,(match-quasiquotation #`(vector-ref #,match-value #,offset) #'pattern
                (loop (add1 offset) #'patterns)
                on-mismatch)
              #,on-mismatch)])))

  (define (match-quasiquotation match-value pattern on-match on-mismatch)
    (syntax-case pattern (unquote unquote-splicing)
      [()
        #`(if (null? #,match-value) #,on-match #,on-mismatch)]

      [#(pattern ...)
        (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
          #`(if (vector? #,match-value)
            #,(match-quasiquoted-vector match-value #'(pattern ...) on-match on-mismatch)
            #,on-mismatch)))]

      [literal (atom? (syntax->datum #'literal))
        #`(if (equal? 'literal #,match-value) #,on-match #,on-mismatch)]

      [,x
        (match-clause match-value #'x on-match on-mismatch)]

      [(,@x)
        (match-clause match-value #'x on-match on-mismatch)]

      [(x . xs)
        (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
          #`(if (pair? #,match-value)
            #,(match-quasiquotation #`(car #,match-value) #'x
              #`(let ([match-value-b (cdr #,match-value)])
                #,(match-quasiquotation #'match-value-b #'xs on-match on-mismatch))
              on-mismatch)
            #,on-mismatch)))]))

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
                          (cons '& (syntax->datum #'unknown-pattern-args)))])]

      [(? . predicate-pattern-args*)
        (syntax-case #'predicate-pattern-args* ()
          [(pattern)
            (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
              #`(if #,match-value
                #,(match-clause match-value #'pattern on-match on-mismatch)
                #,on-mismatch)))]

          [(predicate pattern)
            (wrap-mismatch-in-thunk on-mismatch (lambda (on-mismatch)
              #`(if (predicate #,match-value)
                #,(match-clause match-value #'pattern on-match on-mismatch)
                #,on-mismatch)))]

          [unknown-pattern-args
            (match-errorf "Unexpected predicate pattern ~s, expected (? predicate pattern) or (? pattern)"
                          (cons '? (syntax->datum #'unknown-pattern-args)))])]

      [(-> . view-pattern-args*)
        (syntax-case #'view-pattern-args* ()
          [(procedure pattern)
            (match-clause #`(procedure #,match-value) #'pattern on-match on-mismatch)]

          [unknown-pattern-args
            (match-errorf "Unexpected view pattern ~s, expected (-> procedure pattern)"
                          (cons '-> (syntax->datum #'unknown-pattern-args)))])]

      [(quasiquote pattern)
        (match-quasiquotation match-value #'pattern on-match on-mismatch)]

      [wildcard (pattern-wildcard? (syntax->datum #'wildcard))
        on-match]

      [variable (pattern-variable? (syntax->datum #'variable))
        #`(let ([variable #,match-value]) #,on-match)]

      [literal (pattern-literal? (syntax->datum #'literal))
        #`(if (equal? #,match-value literal) #,on-match #,on-mismatch)]

      [unknown-pattern
        (match-errorf "Unexpected pattern ~s" #'unknown-pattern)]))

  (define (match-clauses macro-args)
    (syntax-case macro-args ()
      [(_)
        #'(void)]

      [(match-value clause . clause*)
        (syntax-case #'clause (?)
          [(pattern (? predicate) on-match on-match* ...)
            (let ([k (if (null? #'(on-match* ...)) #'on-match #`(begin on-match on-match* ...))])
              #`(let ([on-mismatch-thunk (lambda () #,(match-clauses (cons #'match-value #'clause*)))])
                #,(match-clause #'match-value #'pattern #`(if predicate #,k (on-mismatch-thunk)) #'(on-mismatch-thunk))))]

          [(pattern on-match on-match* ...)
            (let ([k (if (null? #'(on-match* ...)) #'on-match #'(begin on-match on-match* ...))])
              (match-clause #'match-value #'pattern k (match-clauses (cons #'match-value #'clause*))))]

          [unknown-clause
            (match-errorf "Unexpected clause ~s, expected [pattern expression ...]" (syntax->datum #'unknown-clause))])]))

  (define (match-wrap-value macro-args)
    (syntax-case macro-args ()
      [()
        (match-errorf "Missing value, expected (match value clause ...)")]

      [(_)
        #'(void)]

      [(macro-arg . macro-args)
        #`(let ([match-value macro-arg]) #,(match-clauses (cons #'match-value #'macro-args)))]))

  (match-wrap-value macro-args))
