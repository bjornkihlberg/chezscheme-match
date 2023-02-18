; (define-syntax if-match
;   (syntax-rules (unquote-splicing quasiquote unquote)
;     [(_ value `,x bodys bodyf) ((lambda (x) bodys) value)]
;     [(_ value `,x t bodys bodyf) ((lambda (x) (if t bodys bodyf)) value)]
;     [(_ value `(,@x) bodys bodyf)
;       (if (list? value) ((lambda (x) bodys) value) bodyf)]
;     [(_ value `(,@x) t bodys bodyf)
;       (if (list? value) ((lambda (x) (if t bodys bodyf)) value) bodyf)]
;     [(_ value `(x) bodys bodyf)
;       (if (and (pair? value) (null? (cdr value)))
;           (if-match (car value) `x bodys bodyf)
;           bodyf)]
;     [(_ value `(x) t bodys bodyf)
;       (if (and (pair? value) (null? (cdr value)))
;           (if-match (car value) `x t bodys bodyf)
;           bodyf)]
;     [(_ value `(x xs ...) bodys bodyf)
;       (if (pair? value)
;           (if-match (car value) `x
;             (if-match (cdr value) `(xs ...) bodys bodyf)
;             bodyf)
;           bodyf)]
;     [(_ value `(x xs ...) t bodys bodyf)
;       (if (pair? value)
;           (if-match (car value) `x
;             (if-match (cdr value) `(xs ...) t bodys bodyf)
;             bodyf)
;           bodyf)]
;     [(_ value `x bodys bodyf) (if (equal? value 'x) bodys bodyf)]
;     [(_ value `x t bodys bodyf) (if (and (equal? value 'x) t) bodys bodyf)]))

; (define-syntax match
;   (syntax-rules (else)
;     [(_ value) (void)]
;     [(_ value (else body)) body]
;     [(_ value (pattern body) rest ...)
;       (if-match value pattern body (match value rest ...))]
;     [(_ value (pattern test body) rest ...)
;       (if-match value pattern test body (match value rest ...))]))

; (define-syntax match-lambda
;   (syntax-rules () [(_ patterns ...) (lambda (x) (match x patterns ...))]))

(define-syntax (match code)
  (define macro-name (car (syntax->datum code)))
  (define macro-args (cdr (syntax->datum code)))

  (define (match-errorf . args)
    (syntax-violation macro-name (string-append (apply format args) " in") code))

  (define (check-clause-syntax clause)
    (unless (pair? clause)
      (match-errorf "Unexpected clause ~s, expected [pattern expression ...]" clause)))

  (define (check-match-syntax macro-args)
    (when (null? macro-args)
      (match-errorf "Missing value, expected (match value clause ...)"))
    (for-each check-clause-syntax (cdr macro-args)))

  ; Check if pattern is variable binding
  (define (pattern-variable? pattern) (symbol? pattern))

  ; Check if pattern is literal comparison
  (define (pattern-literal? pattern)
    (or (and (atom? pattern) (not (null? pattern)))
        (and (pair? pattern) (eq? (car pattern) 'quote))))

  (define (match-clause val pattern on-match on-mismatch)
    (syntax-case pattern (& ? -> quote)
      [(& . named-pattern-args*)
        (syntax-case #'named-pattern-args* ()
          [(pattern0 pattern1 pattern* ...)
            (let ([on-mismatch-thunk (gensym "on-mismatch-thunk")])
              `(let ([,on-mismatch-thunk (lambda () ,on-mismatch)])
                ,(fold-right (lambda (pattern on-match)
                      (match-clause val pattern on-match on-mismatch-thunk))
                    on-match #'(pattern0 pattern1 pattern* ...))))]

          [unknown-pattern-args
            (match-errorf "Unexpected named pattern ~s, expected (& pattern pattern pattern ...)"
                          (cons '& #'unknown-pattern-args))])]

      [(? . predicate-pattern-args*)
        (syntax-case #'predicate-pattern-args* ()
          [(predicate pattern)
            (let ([on-mismatch-thunk (gensym "on-mismatch-thunk")])
              `(let ([,on-mismatch-thunk (lambda () ,on-mismatch)])
                (if (,#'predicate ,val)
                  ,(match-clause val #'pattern on-match `(,on-mismatch-thunk))
                  (,on-mismatch-thunk))))]

          [unknown-pattern-args
            (match-errorf "Unexpected predicate pattern ~s, expected (? predicate pattern)"
                          (cons '? #'unknown-pattern-args))])]

      [(-> . view-pattern-args*)
        (syntax-case #'view-pattern-args* ()
          [(procedure pattern)
            (match-clause `(,#'procedure ,val) #'pattern on-match on-mismatch)]

          [unknown-pattern-args
            (match-errorf "Unexpected view pattern ~s, expected (-> procedure pattern)"
                          (cons '-> #'unknown-pattern-args))])]

      [variable (pattern-variable? #'variable)
        `(let ([,#'variable ,val]) ,on-match)]

      [literal (pattern-literal? #'literal)
        `(if (equal? ,val ,#'literal) ,on-match ,on-mismatch)]

      [unknown-pattern
        (match-errorf "Unexpected pattern ~s" #'unknown-pattern)]))

  (define (match-clauses val . clause*)
    (let ([match-value (gensym "match-value")])
      `(let ([,match-value ,val])
        ,(fold-right (lambda (clause on-mismatch)
          (match-clause match-value (car clause) (cadr clause) on-mismatch))
          '(void)
          clause*))))

  (check-match-syntax macro-args)

  (datum->syntax #'code (apply match-clauses macro-args)))
