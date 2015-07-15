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
   (case (if (> depth 4) (random 3) (random 11))
     [(0) (text "sefsefse")]
     [(1) (rectangle (random 100) (random 100))]
     [(2) (arrow (random 100) (random 100))]
     [(3) (frame (gen))]
     [(4) (cc-superimpose (gen) (gen))]
     [(5) (vl-append (gen) (gen))]
     [(6) (hbl-append (gen) (gen))]
     [(7) (rb-superimpose (gen) (gen))]
     [(8) (panorama (gen))]
     [(9) (scale (gen) (random))]
     [(10) (inset (gen) (random 10) (random 10) (random 10) (random 10))]))
 (for ([i 1000])
   (define p (generate))
   (check-pict=? p (freeze p))))
