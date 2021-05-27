#lang racket/base
(require racket/math
         pict)

(define a (cc-superimpose (rectangle 30 30) (rectangle 10.0 20.0)))
(define b (cc-superimpose (rectangle 30 30) (rectangle 10.0 20.0)))

(define c (frame (rotate (vc-append a b) (- (/ pi 2)))))
(define d (frame (shear (vc-append a b) 0.5 0)))
(define d2 (frame (shear (vc-append a b) -0.5 0.0)))
(define dy (frame (shear (vc-append a b) 0 0.5)))
(define dy2 (frame (shear (vc-append a b) 0 1.5)))

(define (check-cc-find p a x y)
  (define-values (cx cy) (cc-find p a))
  (unless (and (equal? x cx)
               (equal? y cy))
    (error 'check "wrong find result: (~s, ~s) vs. (~s, ~s)" x y cx cy)))

(check-cc-find c a 45.0 15.0)
(check-cc-find d a 22.5 15.0)
(check-cc-find d2 a 37.5 15.0)
(check-cc-find dy a 15.0 22.5)
(check-cc-find dy2 a 15.0 37.5)
