#lang racket/base
(require pict rackunit
         racket/class)

(define (->bitmap p)
   (define b (pict->bitmap p))
   (define its (make-bytes
                (*
                 (send b get-width)
                 (send b get-height)
                 4)
                0))
   (send b get-argb-pixels
         0 0
         (send b get-width)
         (send b get-height)
         its))

(define-binary-check (check-pict=? actual expected)
  (equal? (->bitmap actual) (->bitmap expected)))


(test-case
 "freeze random testing"
 (define (generate [depth 0])
   (define (gen) (generate (add1 depth)))
   (define elems
     (vector (lambda () (text "sefsefse"))
             (lambda () (rectangle (random 100) (random 100)))
             (lambda () (arrow (random 100) (random 100)))
             (lambda () (frame (gen)))
             (lambda () (cc-superimpose (gen) (gen)))
             (lambda () (vl-append (gen) (gen)))
             (lambda () (hbl-append (gen) (gen)))
             (lambda () (rb-superimpose (gen) (gen)))
             (lambda () (panorama (gen)))
             (lambda () (scale (gen) (random)))
             (lambda () (inset (gen) (random 10) (random 10) (random 10) (random 10)))))
   ((vector-ref elems (if (> depth 4) (random 3) (random 10)))))
 (for ([i 1000])
   (define p (generate))
   (check-pict=? p (freeze p))))
