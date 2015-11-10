#lang racket
(require pict
         rackunit
         (for-syntax syntax/parse)
         pict/code)

(unless (equal? (pict-width                                         (code a b))
                (pict-width (code a b)))
  (error "indentation seems to affect a `code` pict's width"))


(define-syntax ccode
  (syntax-parser
    [(_ a b ...)
     #`(#,'code #,(syntax->datum #'a) b ...)]))

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))
(check-not-exn
 (lambda () (eval #'(ccode a b c) ns)))
