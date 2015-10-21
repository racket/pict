#lang racket/base

(require racket/class racket/contract racket/draw
         "convert.rkt"
         pict)

(define color/c
  (or/c string? ;; might be faster
        ;;(and/c string? (lambda (s) (send the-color-database find-color s)))
        (is-a?/c color%)
        (list/c byte? byte? byte?)))

(provide/contract
 [color/c flat-contract?]
 [red     (-> pict-convertible? pict?)]
 [orange  (-> pict-convertible? pict?)]
 [yellow  (-> pict-convertible? pict?)]
 [green   (-> pict-convertible? pict?)]
 [blue    (-> pict-convertible? pict?)]
 [purple  (-> pict-convertible? pict?)]
 [black   (-> pict-convertible? pict?)]
 [brown   (-> pict-convertible? pict?)]
 [gray    (-> pict-convertible? pict?)]
 [white   (-> pict-convertible? pict?)]
 [cyan    (-> pict-convertible? pict?)]
 [magenta (-> pict-convertible? pict?)]
 [light (-> color/c color/c)]
 [dark (-> color/c color/c)])

(define-syntax-rule (define-colors name ...)
  (begin (define (name pict) (colorize pict (symbol->string 'name))) ...))

(define-colors
  red orange yellow green blue purple
  black brown gray white cyan magenta)

(define (light c) (scale-color 2 c))
(define (dark c) (scale-color 1/2 c))
