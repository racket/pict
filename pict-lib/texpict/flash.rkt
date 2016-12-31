#lang racket/base
(require racket/math
         racket/draw
         racket/class
         pict/private/pict)

(provide filled-flash
         outline-flash)

(define-syntax-rule (define-flash id filled?)
  (define (id w h [points 10] [spike-fraction 0.25] [rotation 0])
    (do-flash filled? w h points spike-fraction rotation)))

(define-flash  filled-flash #t)
(define-flash outline-flash #f)

(define (do-flash filled? w h points spike-fraction rotation)
  (define p (new dc-path%))
  (define delta (/ pi points))
  (define in (- 1 spike-fraction))
  (send p move-to 1 0)

  (for/fold ([angle delta])
            ([point (in-range points)])
    (send p line-to (* in (cos angle)) (* in (sin angle)))
    (define new-angle (+ angle delta))
    (send p line-to (cos new-angle) (sin new-angle))
    (+ new-angle delta))

  (send p close)

  (send p scale (/ w 2) (/ h 2))
  (unless (zero? rotation)
    (send p rotate rotation))
  (define-values (bx by bw bh) (send p get-bounding-box))
  (send p translate (- bx) (- by))

  (define no-brush
    (send the-brush-list find-or-create-brush "white" 'transparent))

  (dc (λ (dc x y)
        (define b (send dc get-brush))
        (if filled?
          (send dc set-brush (send (send dc get-pen) get-color) 'solid)
          (send dc set-brush no-brush))
        (send dc draw-path p x y)
        (send dc set-brush b))
      bw bh))
