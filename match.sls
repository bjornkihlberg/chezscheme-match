#!chezscheme

(library (match)
  (export match)

  (import (chezscheme))

  (define-syntax (when-match code)
    (syntax-case code (quasiquote unquote unquote-splicing)
      [(_ value pattern body body* ...) (and (identifier? #'pattern) (free-identifier=? #'pattern #'_))
        #'(begin body body* ...)]

      [(_ value pattern body body* ...) (not (atom? (datum expr)))
        #'(let ([id value])
            (when-match id pattern body body* ...))]

      [(_ value pattern body body* ...) (identifier? #'pattern)
        #'(let ([pattern value]) body body* ...)]

      [(_ value `,pattern body body* ...)
        #'(when-match value pattern body body* ...)]

      [(_ value `(,@pattern) body body* ...)
        #'(when (or (pair? value) (null? value))
            (when-match value pattern body body* ...))]

      [(_ value `(pattern0 . pattern1) body body* ...)
        #'(when (pair? value)
            (when-match (car value) `pattern0
              (when-match (cdr value) `pattern1 body body* ...)))]

      [(_ value (id expr pattern) body body* ...) (identifier? #'id)
        #'(let ([id value])
            (when-match expr pattern body body* ...))]

      [(_ value pattern body body* ...)
        #'(when (equal? value pattern) body body* ...)]))

  (define-syntax (match code)
    (syntax-case code ()
      [(_ value [pattern body body* ...] ...) (not (atom? (datum value)))
        #'(let ([id value]) (match id [pattern body body* ...] ...))]

      [(_ value [pattern body body* ...] ...)
        #`(call/1cc
            (lambda (k)
              (when-match value pattern (call-with-values (lambda () body body* ...) k)) ...
              #,(cond [(syntax->annotation code) =>
                        (lambda (ann)
                          (let-values ([(file line column) (locate-source-object-source (annotation-source ann) #t #f)])
                            #`(assertion-violationf 'match "unmatched value ~s in ~s on line ~s, character ~s" value #,file #,line #,column)))]
                      [else #'(assertion-violationf 'match "unmatched value ~s" value)])))])))
