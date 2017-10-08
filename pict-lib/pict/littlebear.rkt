#lang racket/base

(provide meow)

(require pict racket/runtime-path)

(define-runtime-path LB "private/little-bear.jpg")

(define (meow)
  (bitmap LB))