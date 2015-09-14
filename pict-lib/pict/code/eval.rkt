#lang racket/base

(require pict
         pict/code
         racket/class
         racket/contract
         racket/draw
         racket/sandbox
         racket/string
         (prefix-in scrbl: scribble/eval))

(provide (contract-out
          [current-result-color (parameter/c (or/c string? (is-a?/c color%)))]
          [current-output-color (parameter/c (or/c string? (is-a?/c color%)))]
          [current-error-color (parameter/c (or/c string? (is-a?/c color%)))]
          [get-current-code-interaction-prompt (parameter/c (-> pict?))]
          [typeset-interaction ([] [#:eval procedure?] #:rest (listof syntax?) . ->* . pict?)])
         interaction)

(define current-result-color
  (make-parameter (make-color 0 0 175)))
(define current-output-color
  (make-parameter (make-color 150 0 150)))
(define current-error-color
  (make-parameter (make-color 255 0 0)))

(define get-current-code-interaction-prompt
  (make-parameter (λ () (parameterize ([code-colorize-enabled #f])
                          (typeset-code #'>)))))

(define (typeset-result datum)
  (colorize ((current-code-tt) (format "~v" datum))
            (current-result-color)))

(define (typeset-string str color)
  (let* ([lines (string-split str "\n")]
         [typeset-lines (for/list ([line (in-list lines)])
                          (colorize ((current-code-tt) line) color))])
    (apply vl-append typeset-lines)))

(define (typeset-output str)
  (typeset-string str (current-output-color)))
(define (typeset-error str)
  (typeset-string str (current-error-color)))

(define (eval-with-handlers eval datum)
  (with-handlers ([exn? (λ (exn) (values (void)
                                         (get-output eval)
                                         (exn-message exn)))])
    (let* ([result (eval datum)]
           [output (get-output eval)]
           [error (get-error-output eval)])
      (values result output error))))

(define (typeset-evaluation stx #:eval eval)
  (let-values ([(result output error) (eval-with-handlers eval (syntax->datum stx))])
    (let ([prompt-pict ((get-current-code-interaction-prompt))] 
          [expr-pict (typeset-code stx)]
          [result-pict (if (void? result) (blank) (typeset-result result))]
          [output-pict (if (non-empty-string? output) (typeset-output output) (blank))]
          [error-pict (if (non-empty-string? error) (typeset-error error) (blank))])
      (apply vl-append (list (code #,prompt-pict #,expr-pict)
                             result-pict
                             output-pict
                             error-pict)))))

(define (typeset-interaction #:eval [eval (scrbl:make-base-eval)] . stxs)
  (let ([evaluations (map (λ (stx) (typeset-evaluation stx #:eval eval)) stxs)])
    (apply vl-append evaluations)))

(define-syntax interaction
  (syntax-rules ()
    [(_ #:eval eval expr ...)
     (typeset-interaction #:eval eval (quote-syntax expr) ...)]
    [(_ expr ...)
     (typeset-interaction (quote-syntax expr) ...)]))
