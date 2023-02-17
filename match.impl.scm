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

  (define (check-named-pattern-syntax pattern)
    (let ([l (length pattern)])
      (case l [1 (match-errorf "Unexpected named pattern (@), expected (@ pattern pattern pattern ...)")]
              [2 (match-errorf "Unexpected named pattern (@ ~s), expected (@ pattern pattern pattern ...)" (cadr pattern))])))

  (define (check-view-pattern-syntax pattern)
    (unless (= (length pattern) 3)
      (match-errorf "Unexpected view pattern ~s, expected (-> procedure pattern)" pattern)))

  ; Check if pattern is variable binding
  (define (pattern-variable? pattern) (symbol? pattern))

  ; Check if pattern is literal comparison
  (define (pattern-literal? pattern)
    (or (and (atom? pattern) (not (null? pattern)))
        (and (pair? pattern) (eq? (car pattern) 'quote))))

  ; Check if pattern follows (@ pattern pattern pattern ...)
  (define (pattern-named? pattern)
    (and (pair? pattern) (eq? (car pattern) '@)))

  ; Check if pattern follows (-> procedure pattern)
  (define (pattern-view? pattern)
    (and (pair? pattern) (eq? (car pattern) '->)))

  (define (match-clause val pattern on-match on-mismatch)
    (cond
      [(pattern-variable? pattern)
        `(let ([,pattern ,val]) ,on-match)]

      [(pattern-literal? pattern)
        `(if (equal? ,val ,pattern) ,on-match ,on-mismatch)]

      [(pattern-named? pattern)
        (check-named-pattern-syntax pattern)
        (let ([on-mismatch-thunk (gensym "on-mismatch-thunk")])
          `(let ([,on-mismatch-thunk (lambda () ,on-mismatch)])
            ,(fold-right (lambda (pattern on-match)
                (match-clause val pattern on-match on-mismatch-thunk))
              on-match
              (cdr pattern))))]

      [(pattern-view? pattern)
        (check-view-pattern-syntax pattern)
        (match-clause `(,(cadr pattern) ,val) (caddr pattern) on-match on-mismatch)]

      [else
        (match-errorf "Unexpected pattern ~s" pattern)]))

  (define (match-clauses val . clause*)
    (let ([match-value (gensym "match-value")])
      `(let ([,match-value ,val])
        ,(fold-right (lambda (clause on-mismatch)
          (match-clause match-value (car clause) (cadr clause) on-mismatch))
          '(void)
          clause*))))

  (check-match-syntax macro-args)

  (datum->syntax #'code (apply match-clauses macro-args)))
