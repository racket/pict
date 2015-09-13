#lang racket/base

(require pict
         pict/code
         racket/class
         racket/contract
         racket/draw
         (prefix-in scrbl: scribble/eval))

(provide (contract-out
          [current-result-color (parameter/c (or/c string? (is-a?/c color%)))]
          [get-current-code-interaction-prompt (parameter/c (-> pict?))]
          [typeset-interaction ([] [#:eval procedure?] #:rest (listof syntax?) . ->* . pict?)])
         interaction)

(define current-result-color
  (make-parameter (make-color 0 0 175)))

(define get-current-code-interaction-prompt
  (make-parameter (λ () (parameterize ([code-colorize-enabled #f])
                          (typeset-code #'>)))))

(define (typeset-result datum)
  (parameterize ([code-colorize-enabled #f])
    (colorize ((current-code-tt) (format "~v" datum)) (current-result-color))))

(define (typeset-evaluation stx #:eval eval)
  (let ([typeset-expr (typeset-code stx)]
        [result (eval (syntax->datum stx))])
    (if (void? result)
        (code #,((get-current-code-interaction-prompt)) #,typeset-expr)
        (code #,((get-current-code-interaction-prompt)) #,typeset-expr
              #,(typeset-result result)))))

(define (typeset-interaction #:eval [eval (scrbl:make-base-eval)] . stxs)
  (let ([evaluations (map (λ (stx) (typeset-evaluation stx #:eval eval)) stxs)])
    (apply vl-append evaluations)))

(define-syntax interaction
  (syntax-rules ()
    [(_ #:eval eval expr ...)
     (typeset-interaction #:eval eval #'expr ...)]
    [(_ expr ...)
     (typeset-interaction #'expr ...)]))
