#lang racket/base
(require "private/main.rkt"
         "convert.rkt"
         "private/transform.rkt"
         racket/contract
         racket/class
         racket/draw
         racket/list)

(provide
 (except-out (all-from-out "private/main.rkt")
             blank
             launder
             linewidth
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
  [explain
   (->* (pict-convertible?)
        (#:border (or/c #f string? (is-a?/c color%))
         #:ascent (or/c #f string? (is-a?/c color%))
         #:baseline (or/c #f string? (is-a?/c color%))
         #:scale real?
         #:line-width real?)
        pict?)]
  [explain-child
   (->* (pict-convertible?)
        (#:border (or/c #f string? (is-a?/c color%))
         #:ascent (or/c #f string? (is-a?/c color%))
         #:baseline (or/c #f string? (is-a?/c color%))
         #:scale real?
         #:line-width real?)
        #:rest (listof pict-path?)
        pict?)]
  [launder (-> pict-convertible? pict-convertible?)]
  [blank
   (case->
    (-> pict?)
    (-> real? pict?)
    (-> real? real? pict?)
    (-> real? real? real? pict?)
    (-> real? real? real? real? pict?))]
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
              #:pre/desc (ncols picts)
              (let ((rem (remainder (length picts) ncols)))
                (or (zero? rem)
                    (format "ncols does not divide number of picts (~a % ~a = ~a)" (length picts) ncols rem)))
              [result pict?])]
  [dc (->i ([draw (-> (is-a?/c dc<%>) real? real? any)]
            [w real?]
            [h real?])
           ([d (or/c #f real?)]
            [a (or/c #f real?)])
           #:pre/name (draw)
           "draw proc does not restore the dc state after being called"
           (does-draw-restore-the-state-after-being-called? draw)
           [p pict?])]
  [rename dc unsafe-dc
          (->i ([draw (-> (is-a?/c dc<%>) real? real? any)]
                [w real?]
                [h real?])
               ([d (or/c #f real?)]
                [a (or/c #f real?)])
               [p pict?])]
  [cellophane (-> pict-convertible? (real-in 0 1) pict?)]

  [linewidth (-> (or/c real? #f) pict-convertible? pict?)]

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
                     ((or/c 'unsmoothed 'smoothed 'aligned)
                      #:make-bitmap (-> exact-positive-integer? exact-positive-integer? (is-a?/c bitmap%)))
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

(define (does-draw-restore-the-state-after-being-called? draw)
  (define bdc (new bitmap-dc% [bitmap (make-bitmap 1 1)]))
  (prandomize-state bdc)
  (define old-state (get-dc-state bdc))
  (draw bdc 0 0)
  (equal? (get-dc-state bdc) old-state))

;; randomizes some portions of the state of the given dc;
;; doesn't pick random values for things that the 'dc'
;; function promises not to change (e.g. the pen/brush style).
(define (prandomize-state dc)
  (send dc set-origin (prandom-real) (prandom-real))
  (send dc set-pen (prandom-color) (prandom 255) 'solid)
  (send dc set-brush (prandom-color) 'solid)
  (send dc set-alpha (prandom))
  (send dc set-text-background (prandom-color))
  (send dc set-text-foreground (prandom-color))
  (send dc set-text-mode 'transparent)
  (send dc set-font (send the-font-list find-or-create-font
                          (+ 1 (prandom 254))
                          (pick-one 'default 'decorative 'roman 'script
                                    'swiss 'modern 'symbol 'system)
                          (pick-one 'normal 'italic 'slant)
                          (pick-one 'normal 'bold 'light)))
  ;; set-transformation is relatively expensive
  ;; at the moment, so we don't randomize it
  #;
  (send dc set-transformation
        (vector (vector (prandom-real) (prandom-real) (prandom-real)
                        (prandom-real) (prandom-real) (prandom-real))
                (prandom-real) (prandom-real) (prandom-real) (prandom-real) (prandom-real))))

(define (prandom-real) (+ (prandom 1000) (prandom)))
(define (prandom-color) (make-object color% (prandom 255) (prandom 255) (prandom 255)))
(define (pick-one . args) (list-ref args (prandom (length args))))
(define pict-psrg
  (make-pseudo-random-generator))
(define prandom
  (case-lambda
    [()
     (parameterize ([current-pseudo-random-generator pict-psrg])
       (random))]
    [(x)
     (parameterize ([current-pseudo-random-generator pict-psrg])
       (random x))]))

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

(define (explain p         
                 #:border [b "Firebrick"]
                 #:ascent [a "MediumSeaGreen"]
                 #:baseline [d "DodgerBlue"]
                 #:scale [s 5]
                 #:line-width [lw 1])
  (explain-child* p p b a d s lw))
(define (explain-child
         p
         #:border [b "Firebrick"]
         #:ascent [a "MediumSeaGreen"]
         #:baseline [d "DodgerBlue"]
         #:scale [s 5]
         #:line-width [lw 1]
         . child-path)
  (scale
   (for/fold ([p p])
             ([c (in-list child-path)])
     (explain-child* p c b a d 1 lw))
   s))
(define (explain-child*
         p
         child-path
         b a d s lw)
  (define t (get-child-transformation p child-path))
  (define child (last (flatten child-path)))
  (define cw (pict-width child))
  (define ch (pict-height child))
  (define nw (+ (* 2 lw) (pict-width p)))
  (define nh (+ (* 2 lw) (pict-height p)))
  (define ncw (+ (* 2 lw) cw))
  (define nch (+ (* 2 lw) ch))
  (define lw/2 (/ lw 2))
  (define annotations
    (dc
     (lambda (dc dx dy)
       (define oldt (send dc get-transformation))
       (define p (send dc get-pen))
       (define br (send dc get-brush))
       (send dc set-brush "white" 'transparent)
       (send dc translate dx dy)
       (send dc transform t)
       (when b
         (define t2 (send dc get-transformation))
         (send dc scale (/ ncw cw) (/ nch ch))
         (define path (new dc-path%))
         (send dc set-pen b lw 'solid)
         (send path move-to lw/2 lw/2)
         (send path line-to lw/2 (- ch lw/2))
         (send path line-to (- cw lw/2) (- ch lw/2))
         (send path line-to (- cw lw/2) lw/2)
         (send path close)
         (send dc draw-path path 0 0)
         (send dc set-transformation t2))
       (when a
         (define line (pict-ascent child))
         (send dc set-pen a lw 'solid)
         (send dc draw-line lw/2 line
               (+ lw lw/2 cw) line))
       (when d
         (define line (- (pict-height child) (pict-descent child)))
         (send dc set-pen d lw 'solid)
         (send dc draw-line lw/2 line
               (+ lw lw/2 cw) line))
       (send dc set-transformation oldt)
       (send dc set-pen p)
       (send dc set-brush br))
     nw nh))
  (scale
   (cc-superimpose
    p
    annotations)
   s))


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
