#lang racket/base

(require racket/class racket/contract racket/draw
         pict)

(define color/c
  (or/c string? ;; might be faster
        ;;(and/c string? (lambda (s) (send the-color-database find-color s)))
        (is-a?/c color%)
        (list/c byte? byte? byte?)))

(provide/contract
 [color/c flat-contract?]
 [red     (-> pict? pict?)]
 [orange  (-> pict? pict?)]
 [yellow  (-> pict? pict?)]
 [green   (-> pict? pict?)]
 [blue    (-> pict? pict?)]
 [purple  (-> pict? pict?)]
 [black   (-> pict? pict?)]
 [brown   (-> pict? pict?)]
 [gray    (-> pict? pict?)]
 [white   (-> pict? pict?)]
 [cyan    (-> pict? pict?)]
 [magenta (-> pict? pict?)]
 [light (-> color/c color/c)]
 [dark (-> color/c color/c)])

(define-syntax-rule (define-colors name ...)
  (begin (define (name pict) (colorize pict (symbol->string 'name))) ...))

(define-colors
  red orange yellow green blue purple
  black brown gray white cyan magenta)

(define (light c) (scale-color 2 c))
(define (dark c) (scale-color 1/2 c))
