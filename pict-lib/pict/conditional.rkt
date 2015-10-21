#lang racket/base

(require pict
         racket/contract/base
         racket/stxparam
         "convert.rkt"
         (for-syntax racket/base))

(provide/contract
 [hide (->* [pict-convertible?] [any/c] pict?)]
 [show (->* [pict-convertible?] [any/c] pict-convertible?)])
(provide pict-if pict-cond pict-case)

;; The original API in unstable/gui/pict provided a syntax parameter to control
;; the default combiner. This made the API more complex, and potentially scary
;; due to its use of a high-powered features. To keep things simpler, the
;; pict/conditional API does not expose those, but they need to be exposed via
;; unstable/gui/pict for backwards compatibility.
(module params racket/base
  (require racket/splicing racket/stxparam pict
           (for-syntax racket/base))
  (provide pict-combine with-pict-combine)
  (define-syntax-parameter pict-combine #'ltl-superimpose)
  (define-syntax-rule (with-pict-combine combine body ...)
    (splicing-syntax-parameterize
     ([pict-combine #'combine])
     body ...)))

(require (submod "." params))

(define-syntax (pict-if stx)
  (syntax-case stx ()
    [(_ #:combine combine test success failure)
     (syntax/loc stx
       (let* ([result test])
         (combine (show success result)
                  (hide failure result))))]
    [(_ test success failure)
     (quasisyntax/loc stx
       (pict-if #:combine #,(syntax-parameter-value #'pict-combine)
                test success failure))]))

(define-syntax (pict-cond stx)
  (syntax-case stx (else)
    [(_ #:combine combine [test expr] ... [else default])
     (with-syntax ([(pict ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([pict expr] ... [final default])
           (combine (cond [test pict] ... [else final])
                    (ghost pict) ... (ghost final)))))]
    [(_ #:combine combine [test pict] ...)
     (syntax/loc stx
       (pict-cond #:combine combine [test pict] ... [else (blank 0 0)]))]
    [(_ [test expr] ...)
     (quasisyntax/loc stx
       (pict-cond #:combine #,(syntax-parameter-value #'pict-combine)
                  [test expr] ...))]))

(define-syntax (pict-case stx)
  (syntax-case stx (else)
    [(_ test #:combine combine [literals expr] ... [else default])
     (with-syntax ([(pict ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (let ([pict expr] ... [final default])
           (combine (case test [literals pict] ... [else final])
                    (ghost pict) ... (ghost final)))))]
    [(_ test #:combine combine [literals expr] ...)
     (syntax/loc stx
       (pict-case test #:combine combine
                  [literals expr] ... [else (blank 0 0)]))]
    [(_ test [literals expr] ...)
     (quasisyntax/loc stx
       (pict-case test #:combine #,(syntax-parameter-value #'pict-combine)
                  [literals expr] ...))]))

(define (hide pict [hide? #t])
  (if hide? (ghost pict) pict))

(define (show pict [show? #t])
  (if show? pict (ghost pict)))
