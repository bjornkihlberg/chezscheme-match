#!chezscheme

(library (match)
  (export match -> @ ?)

  (import (chezscheme))

  (define-syntax (-> code) (syntax-error code "misplaced aux keyword"))

  (define-syntax (@ code) (syntax-error code "misplaced aux keyword"))

  (define-syntax (? code) (syntax-error code "misplaced aux keyword"))

  (define-syntax (when-match code)
    (syntax-case code (quasiquote unquote unquote-splicing -> @ ?)
      [(_ value pattern body body* ...) (identifier? #'pattern)
        (if (free-identifier=? #'pattern #'_)
            #'(begin body body* ...)
            #'(let ([pattern value]) body body* ...))]

      [(_ value `,pattern body body* ...)
        #'(when-match value pattern body body* ...)]

      [(_ value `(,@pattern) body body* ...)
        #'(when (list? value)
            (when-match value pattern body body* ...))]

      [(_ value `(pattern pattern* ... . patterns) body body* ...)
        #'(when (pair? value)
            (let ([head (car value)]
                  [tail (cdr value)])
              (when-match head `pattern
                (when-match tail `(pattern* ... . patterns) body body* ...))))]

      [(_ value (-> expr pattern) body body* ...)
        #'(let ([x (expr value)])
            (when-match x pattern body body* ...))]

      [(_ value (@ pattern0 pattern1 pattern* ...) body body* ...)
        #`(when-match value pattern0
            #,(let loop ([pattern #'pattern1] [patterns #'(pattern* ...)])
                (if (null? patterns)
                    #`(when-match value #,pattern body body* ...)
                    #`(when-match value #,pattern #,(loop (car patterns) (cdr patterns))))))]

      [(_ value (? predicate pattern) body body* ...)
        #'(when (predicate value) (when-match value pattern body body* ...))]

      [(_ value pattern body body* ...)
        #'(when (equal? value pattern) body body* ...)]))

  (define-syntax (match code)
    (syntax-case code (?)
      [(_ _) #'(void)]

      [(_ value . clauses) (not (identifier? #'value))
        #'(let ([id value]) (match id . clauses))]

      [(_ value [pattern body body* ...]) (identifier? #'value)
        #'(when-match value pattern body body* ...)]

      [(m value [pattern body body* ...] ...) (identifier? #'value)
        #`(call/1cc
            (lambda (return)
              #,@(map
                  (lambda (clause)
                    (syntax-case clause (?)
                      [[pattern (? test+ body+) ...]
                        #'(when-match value pattern
                            (call-with-values
                              (lambda () (cond [test+ body+] ...))
                              return))]
                      [[pattern body+ ...]
                        #'(when-match value pattern
                            (call-with-values
                              (lambda () body+ ...)
                              return))]))
                  #'([pattern body body* ...] ...))))])))
