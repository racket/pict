#lang racket/base
(require "private/pict.rkt"
         racket/contract/base
         racket/generic
         (prefix-in prop-based: "private/convertible.rkt"))

(provide gen:pict-convertible pict-convertible?
         pict-convert
         (rename-out [pict-convertible/c* pict-convertible/c]))

(define-generics pict-convertible

  (pict-convert pict-convertible)
  
  #:derive-property prop-based:prop:pict-convertible? (λ (x) #t)
  #:derive-property prop-based:prop:pict-convertible pict-convert)

(define pict-convertible/c*
  (pict-convertible/c [pict-convert (-> pict-convertible? pict?)]))
