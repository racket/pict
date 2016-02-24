#lang racket/base
(require "private/main.rkt"
         "convert.rkt"
         racket/contract
         racket/class
         racket/draw
         racket/bool)

(define a-number 0)

(provide
 (except-out (all-from-out "private/main.rkt")
             use-last
             use-last*
             pict->bitmap
             pict->argb-pixels
             argb-pixels->pict
             colorize
             pin-under pin-over
             rectangle filled-rectangle
             rounded-rectangle filled-rounded-rectangle
             circle disk ellipse filled-ellipse
             lt-find
             lc-find
             lb-find
             ltl-find
             lbl-find
             ct-find
             cc-find
             cb-find
             ctl-find
             cbl-find
             rt-find
             rc-find
             rb-find
             rtl-find
             rbl-find
             vl-append
             vc-append
             vr-append
             ht-append
             hc-append
             hb-append
             htl-append
             hbl-append
           pin-line pin-arrow-line pin-arrows-line
             cellophane
             frame
             dc
             table)

 pin-arrow-line
 pin-arrows-line
 pin-line
 (contract-out
  [frame (->* (pict-convertible?)
              (#:segment (or/c #f real?)
               #:color (or/c #f string? (is-a?/c color%))
               #:line-width (or/c #f real?))
              pict?)]
  [table (->i ([ncols exact-positive-integer?]
               [picts (non-empty-listof pict-convertible?)]
               [col-aligns (or/c (list*of (->* () #:rest (listof pict-convertible?) pict-convertible?))
                                 (listof (->* () #:rest (listof pict-convertible?) pict-convertible?)))]
               [row-aligns (or/c (list*of (->* () #:rest (listof pict-convertible?) pict-convertible?))
                                 (listof (->* () #:rest (listof pict-convertible?) pict-convertible?)))]
               [col-seps (or/c (listof real?) (list*of real?))]
               [row-seps (or/c (listof real?) (list*of real?))])
              #:pre (ncols picts)
              (zero? (remainder (length picts) ncols))
              [result pict?])]
  [dc (->i ([draw (-> (is-a?/c dc<%>) real? real? any)]
            [w real?]
            [h real?])
           ([d (or/c #f real?)]
            [a (or/c #f real?)])
           #:pre (draw) (does-draw-restore-the-state-after-being-called? draw)
           [p pict?])]
  [cellophane (-> pict-convertible? (real-in 0 1) pict?)]

  [lt-find  *-find/c]
  [lc-find  *-find/c]
  [lb-find  *-find/c]
  [ltl-find *-find/c]
  [lbl-find *-find/c]
  [ct-find  *-find/c]
  [cc-find  *-find/c]
  [cb-find  *-find/c]
  [ctl-find *-find/c]
  [cbl-find *-find/c]
  [rt-find  *-find/c]
  [rc-find  *-find/c]
  [rb-find  *-find/c]
  [rtl-find *-find/c]
  [rbl-find *-find/c]
  [vl-append *-append/c]
  [vc-append *-append/c]
  [vr-append *-append/c]
  [ht-append *-append/c]
  [hc-append *-append/c]
  [hb-append *-append/c]
  [htl-append *-append/c]
  [hbl-append *-append/c]

  [use-last (-> pict-convertible? pict-path? pict?)]
  [use-last* (-> pict-convertible? pict-convertible? pict?)]

  [colorize (-> pict-convertible? 
                (or/c string? 
                      (is-a?/c color%)
                      (list/c byte? byte? byte?))
                pict?)]

  [pict->bitmap (->* (pict-convertible?)
                     ((or/c 'unsmoothed 'smoothed 'aligned))
                     (is-a?/c bitmap%))]
  [pict->argb-pixels (->* (pict-convertible?)
                          ((or/c 'unsmoothed 'smoothed 'aligned))
                          (and/c bytes? multiple-of-four-bytes?))]
  [argb-pixels->pict (-> (and/c bytes? multiple-of-four-bytes?)
                         exact-nonnegative-integer?
                         pict?)]
  [pin-under
   (->i ([base pict-convertible?]
         [dx/fp (or/c real? pict-path?)]
         [dy/f (dx/fp)
               (if (real? dx/fp)
                   real?
                   (-> pict-convertible? pict-path? (values real? real?)))]
         [pict pict-convertible?])
        [result pict?])]
  [pin-over
   (->i ([base pict-convertible?]
         [dx/fp (or/c real? pict-path?)]
         [dy/f (dx/fp)
               (if (real? dx/fp)
                   real?
                   (-> pict-convertible? pict-path? (values real? real?)))]
         [pict pict-convertible?])
        [result pict?])]
  [rectangle (->* ((and/c rational? (not/c negative?))
                   (and/c rational? (not/c negative?)))
                  (#:border-color (or/c #f string? (is-a?/c color%))
                   #:border-width (or/c #f (and/c rational? (not/c negative?))))
                  pict?)]
  [filled-rectangle (->i ([w (and/c rational? (not/c negative?))]
                          [h (and/c rational? (not/c negative?))])
                         (#:draw-border? [draw-border? any/c]
                          #:color        [color (or/c #f string? (is-a?/c color%))]
                          #:border-color [border-color (or/c #f string? (is-a?/c color%))]
                          #:border-width [border-width (or/c #f (and/c rational? (not/c negative?)))])
                         #:pre (draw-border? border-color border-width)
                         (if (not draw-border?)
                             (and (or (unsupplied-arg? border-color)
                                      (not border-color))
                                  (or (unsupplied-arg? border-width)
                                      (not border-width)))
                             #t)
                         [_ pict?])]
  [rounded-rectangle (->* ((and/c rational? (not/c negative?))
                           (and/c rational? (not/c negative?)))
                          (rational?
                           #:angle rational?
                           #:border-color (or/c #f string? (is-a?/c color%))
                           #:border-width (or/c #f (and/c rational? (not/c negative?))))
                          pict?)]
  [filled-rounded-rectangle (->i ([w (and/c rational? (not/c negative?))]
                                  [h (and/c rational? (not/c negative?))])
                                 ([corner-radius rational?]
                                  #:angle        [angle rational?]
                                  #:draw-border? [draw-border? any/c]
                                  #:color        [color (or/c #f string? (is-a?/c color%))]
                                  #:border-color [border-color (or/c #f string? (is-a?/c color%))]
                                  #:border-width [border-width (or/c #f (and/c rational? (not/c negative?)))])
                                 #:pre (draw-border? border-color border-width)
                                 (if (not draw-border?)
                                     (and (or (unsupplied-arg? border-color)
                                              (not border-color))
                                          (or (unsupplied-arg? border-width)
                                              (not border-width)))
                                     #t)
                                 [_ pict?])]
  [circle (->* ((and/c rational? (not/c negative?)))
               (#:border-color (or/c #f string? (is-a?/c color%))
                #:border-width (or/c #f (and/c rational? (not/c negative?))))
               pict?)]
  [disk (->i ([r (and/c rational? (not/c negative?))])
             (#:draw-border? [draw-border? any/c]
              #:color        [color (or/c #f string? (is-a?/c color%))]
              #:border-color [border-color (or/c #f string? (is-a?/c color%))]
              #:border-width [border-width (or/c #f (and/c rational? (not/c negative?)))])
             #:pre (draw-border? border-color border-width)
             (if (not draw-border?)
                 (and (or (unsupplied-arg? border-color)
                          (not border-color))
                      (or (unsupplied-arg? border-width)
                          (not border-width)))
                 #t)
             [_ pict?])]
  [ellipse (->* ((and/c rational? (not/c negative?))
                 (and/c rational? (not/c negative?)))
                (#:border-color (or/c #f string? (is-a?/c color%))
                 #:border-width (or/c #f (and/c rational? (not/c negative?))))
                pict?)]
  [filled-ellipse (->i ([w (and/c rational? (not/c negative?))]
                        [h (and/c rational? (not/c negative?))])
                       (#:draw-border? [draw-border? any/c]
                        #:color        [color (or/c #f string? (is-a?/c color%))]
                        #:border-color [border-color (or/c #f string? (is-a?/c color%))]
                        #:border-width [border-width (or/c #f (and/c rational? (not/c negative?)))])
                       #:pre (draw-border? border-color border-width)
                       (if (not draw-border?)
                           (and (or (unsupplied-arg? border-color)
                                    (not border-color))
                                (or (unsupplied-arg? border-width)
                                    (not border-width)))
                           #t)
                       [_ pict?])]))

(define find/c (pict? pict-path? . -> . (values real? real?)))

(define (does-draw-restore-the-state-after-being-called? draw)
  (define bdc (new bitmap-dc% [bitmap (make-bitmap 1 1)]))
  (randomize-state bdc)
  (define old-state (get-dc-state bdc))
  (draw bdc 0 0)
  (equal? (get-dc-state bdc) old-state))

;; randomizes some portions of the state of the given dc;
;; doesn't pick random values for things that the 'dc'
;; function promises not to change (e.g. the pen/brush style).
(define (randomize-state dc)
  (send dc set-origin (random-real) (random-real))
  (send dc set-pen (random-color) (random 255) 'solid)
  (send dc set-brush (random-color) 'solid)
  (send dc set-alpha (random))
  (send dc set-text-background (random-color))
  (send dc set-text-foreground (random-color))
  (send dc set-text-mode 'transparent)
  (send dc set-font (send the-font-list find-or-create-font
                          (+ 1 (random 254))
                          (pick-one 'default 'decorative 'roman 'script
                                    'swiss 'modern 'symbol 'system)
                          (pick-one 'normal 'italic 'slant)
                          (pick-one 'normal 'bold 'light)))
  ;; set-transformation is relatively expensive
  ;; at the moment, so we don't randomize it
  #;
  (send dc set-transformation
        (vector (vector (random-real) (random-real) (random-real)
                        (random-real) (random-real) (random-real))
                (random-real) (random-real) (random-real) (random-real) (random-real))))

(define (random-real) (+ (random 1000) (random)))
(define (random-color) (make-object color% (random 255) (random 255) (random 255)))
(define (pick-one . args) (list-ref args (random (length args))))

(define (get-dc-state dc)
  (vector (pen->vec (send dc get-pen))
          (brush->vec (send dc get-brush))
          (send dc get-alpha)
          (font->vec (send dc get-font))
          (let-values ([(ox oy) (send dc get-origin)])
            (cons ox oy))
          (color->vec (send dc get-text-background))
          (send dc get-text-mode)
          (send dc get-transformation)
          (color->vec (send dc get-text-foreground))))

(define (pen->vec pen)
  (vector (color->vec (send pen get-color))
          (send pen get-width)
          (send pen get-style)))

(define (brush->vec brush)
  (vector (color->vec (send brush get-color))
          (send brush get-style)))

(define (font->vec font)
  (vector (send font get-point-size)
          (send font get-family)
          (send font get-style)
          (send font get-weight)))

(define (color->vec c)
  (vector (send c red) (send c green) (send c blue)))

(define *-find/c
  (-> pict-convertible? pict-path? (values real? real?)))

(define *-append/c
  (->* ()
       ()
       #:rest (or/c (cons/c real? (listof pict-convertible?))
                    (listof pict-convertible?))
       pict-convertible?))

(define (multiple-of-four-bytes? b)
  (zero? (modulo (bytes-length b) 4)))

(require "private/play-pict.rkt")
(provide
 (contract-out
  [fade-pict (->* ((real-in 0.0 1.0) pict-convertible? pict-convertible?) (#:combine (-> pict-convertible? pict-convertible? pict?)) pict?)]
  [slide-pict (-> pict-convertible? pict-convertible? pict-convertible? pict-convertible? (real-in 0.0 1.0) pict?)]
  [slide-pict/center (-> pict-convertible? pict-convertible? pict-convertible? pict-convertible? (real-in 0.0 1.0) pict?)]
  [fade-around-pict (-> (real-in 0.0 1.0) pict-convertible? (-> pict-convertible? pict?) pict?)]
  [sequence-animations (->* () #:rest (listof (-> (real-in 0.0 1.0) pict-convertible?))
                            (-> (real-in 0.0 1.0) pict?))]
  [reverse-animations (->* () #:rest (listof (-> (real-in 0.0 1.0) pict-convertible?))
                           (-> (real-in 0.0 1.0) pict?))]
  [fast-start (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-end (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-edges (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [fast-middle (-> (real-in 0.0 1.0) (real-in 0.0 1.0))]
  [split-phase (-> (real-in 0.0 1.0) (values (real-in 0.0 1.0) (real-in 0.0 1.0)))]))
