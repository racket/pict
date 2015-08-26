#lang racket
(require pict rackunit
         (for-syntax syntax/parse))

(define (->bitmap p)
  (define b (pict->bitmap p))
  (define w (send b get-width))
  (define h (send b get-height))

   (define its (make-bytes
                (*
                 w h
                 4)
                255))
   (send b get-argb-pixels
         0 0
         (send b get-width)
         (send b get-height)
         its)
   (define mask (send b get-loaded-mask))
   (when mask
     (send b get-argb-pixels 0 0 w h its #t))
  its)

(define-check (check-pict=? actual expected msg)
  (unless (equal? (->bitmap actual) (->bitmap expected))
    (fail-check msg)))

(define-syntax (gen-case stx)
  (syntax-parse stx
    [(_ e:expr [(n) (m:id b:expr ...)] ...)
     (with-syntax ([((i ...) ...) (map generate-temporaries (syntax->list #'((b ...) ...)))])
       #`(case e
           [(n)
            (define i (call-with-values (lambda () b) list)) ...
            (values
             `(m ,(first i) ...)
             (m (if (null? (rest i)) (first i) (second i)) ...))]
           ...))]))

(define (generate-pict)
  (define-values (l p)
  (let loop ([depth 0])
    (define (gen) (loop (add1 depth)))
    (gen-case
     (if (> depth 4) (random 3) (random 11))
     [(0) (text "sefsefse")]
     [(1) (rectangle (random 10) (random 10))]
     [(2) (arrow (random 10) (random 10))]
     [(3) (frame (gen))]
     [(4) (cc-superimpose (gen) (gen))]
     [(5) (vl-append (gen) (gen))]
     [(6) (hbl-append (gen) (gen))]
     [(7) (rb-superimpose (gen) (gen))]
     [(8) (panorama (gen))]
     [(9) (scale (gen) (random))]
     [(10) (inset (gen) (random 10) (random 10) (random 10) (random 10))])))
  (values l (cc-superimpose (blank 200) p)))

(test-case
 "freeze random testing"
 (for ([i 1000])
   (define-values (l p) (generate-pict))
   (check-pict=? p (freeze p) (format "~a" l))))
