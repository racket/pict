#lang racket/base
(require (for-syntax racket/base)
         racket/unsafe/ops
         racket/contract/base
         racket/class
         racket/draw
         racket/future
         racket/math
         pict)

(define nneg-real/c (and/c real? (not/c negative?)))

(provide/contract
 [blur
  (->* (pict? nneg-real/c)
       (nneg-real/c
        #:pre-inset? any/c)
       pict?)]
 [shadow
  (->* (pict? nneg-real/c)
       (real? real?
        #:color (or/c #f string? (is-a?/c color%))
        #:shadow-color (or/c #f string? (is-a?/c color%)))
       pict?)]
 [shadow-frame
  (->* ()
       (#:background-color (or/c string? (is-a?/c color%))
        #:frame-color (or/c string? (is-a?/c color%))
        #:frame-line-width (or/c real? #f)
        #:shadow-side-length real?
        #:shadow-top-y-offset real?
        #:shadow-bottom-y-offset real?
        #:shadow-descent (and/c real? (not/c negative?))
        #:shadow-alpha-factor real?
        #:blur (and/c real? (not/c negative?))
        #:margin real?
        #:sep real?)
       #:rest (listof pict?)
       pict?)])

;; ----

(define (blur p hbr [vbr hbr]
              #:pre-inset? [pre-inset? #t])
  (let* ([p
          (cond [pre-inset? (inset p hbr vbr)]
                [else p])]
         [blurred (*blur p hbr vbr)])
    (cond [pre-inset? (inset blurred (- hbr) (- vbr))]
          [else blurred])))

(define (shadow p br [dx 0] [dy dx]
                #:color [c #f]
                #:shadow-color [shc #f]
                #:auto-inset? [auto-inset? #f])
  ;; FIXME: should auto-inset also use dx, dy?
  (define (colorize* p c)
    (if c (colorize p c) p))
  (let ([result
         (pin-under (colorize* p c)
                    dx dy
                    (blur (colorize* p shc) br))])
    (cond [auto-inset? (inset result br)]
          [else result])))

;; ----

(define MAX-RADIUS (expt 2 10))
(define MAX-WEIGHT (expt 2 5))
(define BOX-ITERATIONS 3)

(define (*blur p hbr vbr)
  (let* ([w (pict-width p)]
         [h (pict-height p)]
         [drawer (make-pict-drawer p)])
    (dc (lambda (dc x y)
          (let-values ([(sx sy) (send dc get-scale)])
            (let* ([pxw (ceil/e (* w sx))]
                   [pxh (ceil/e (* h sy))]
                   [hbr* (min (ceil/e (* hbr sx)) pxw MAX-RADIUS)]
                   [vbr* (min (ceil/e (* vbr sy)) pxh MAX-RADIUS)]
                   [bmp (make-object bitmap% pxw pxh #f #t)]
                   [bdc (new bitmap-dc% (bitmap bmp))])
              (send bdc set-scale sx sy)
              (send bdc set-font (send dc get-font))
              (send bdc set-pen (send dc get-pen))
              (send bdc set-brush (send dc get-brush))
              (send bdc set-text-foreground (send dc get-text-foreground))
              (when (or (zero? hbr*) (zero? vbr*))
                ;; probably not worth smoothing when about to blur
                ;; except when blurring by zero
                (send bdc set-smoothing (send dc get-smoothing)))
              (drawer bdc 0 0)
              (blur! bmp hbr* vbr*)
              (send dc set-scale 1.0 1.0)
              (send dc draw-bitmap bmp (* x sx) (* y sy))
              (send dc set-scale sx sy))))
        w h)))

(define (blur! bmp hbr vbr)
  (let* ([w (send bmp get-width)]
         [h (send bmp get-height)]
         [pix (make-bytes (* w h 4))]
         [out (make-bytes (* w h 4))])
    (send bmp get-argb-pixels 0 0 w h pix #f #t)
    (let ([hbr (ceil/e (/ hbr BOX-ITERATIONS))]
          [vbr (ceil/e (/ vbr BOX-ITERATIONS))])
      (box-h pix out hbr w h BOX-ITERATIONS)
      (let-values ([(pix* out*)
                    (cond [(even? BOX-ITERATIONS) (values out pix)]
                          [else (values pix out)])])
        (box-v pix* out* vbr w h BOX-ITERATIONS)))
    (send bmp set-argb-pixels 0 0 w h pix #f #t)
    (void)))

;; ----

;; iterated box blur

(define-syntax-rule (box-line* radius start end get-val set-val)
  (let ([non-zero-alpha?
         (for/or ([outi (in-range start end)])
           (positive? (get-val outi 0)))])
    (cond [non-zero-alpha?
           (for/fold ([wA 0] [wR 0] [wG 0] [wB 0] [wW 0])
             ([leadI (in-range start (+ end radius))])
             ;; (eprintf "leadI = ~s, wA = ~s, wW = ~s\n" leadI wA wW)
             (let*-values ([(outI) (unsafe-fx- leadI radius)]
                           [(tailI) (unsafe-fx- leadI (unsafe-fx+ radius radius))]
                           [(addA addR addG addB addW)
                            (cond [(unsafe-fx< leadI end)
                                   (values (get-val leadI 0)
                                           (get-val leadI 1)
                                           (get-val leadI 2)
                                           (get-val leadI 3)
                                           1)]
                                  [else (values 0 0 0 0 0)])]
                           [(dropA dropR dropG dropB dropW)
                            (cond [(unsafe-fx>= tailI start)
                                   (values (get-val tailI 0)
                                           (get-val tailI 1)
                                           (get-val tailI 2)
                                           (get-val tailI 3)
                                           1)]
                                  [else (values 0 0 0 0 0)])]
                           [(nwA) (unsafe-fx+ wA addA)]
                           [(nwR) (unsafe-fx+ wR addR)]
                           [(nwG) (unsafe-fx+ wG addG)]
                           [(nwB) (unsafe-fx+ wB addB)]
                           [(nwW) (unsafe-fx+ wW addW)])
               (when (and (unsafe-fx>= outI start) (unsafe-fx< outI end))
                 ;; (eprintf "setting ~a = (~a,...)\n" outI (quotient nwA nwW))
                 (set-val outI 0 (unsafe-fxquotient nwA nwW))
                 (set-val outI 1 (unsafe-fxquotient nwR nwW))
                 (set-val outI 2 (unsafe-fxquotient nwG nwW))
                 (set-val outI 3 (unsafe-fxquotient nwB nwW)))
               (values (unsafe-fx- nwA dropA)
                       (unsafe-fx- nwR dropR)
                       (unsafe-fx- nwG dropG)
                       (unsafe-fx- nwB dropB)
                       (unsafe-fx- nwW dropW))))]
          [else
           (for ([outI (in-range start end)])
             (set-val outI 0 0)
             (set-val outI 1 0)
             (set-val outI 2 0)
             (set-val outI 3 0))])))

(define (box-h in out radius w h iterations)
  (for/async ([row (in-range h)])
    (for ([iter (in-range iterations)])
      (let ([start (* row w)]
            [end (* (add1 row) w)]
            [in (if (even? iter) in out)]
            [out (if (even? iter) out in)])
        (define-syntax-rule (get-val i offset)
          (bytes-ref in (unsafe-fx+ offset (unsafe-fx* 4 i))))
        (define-syntax-rule (set-val i offset v)
          (bytes-set! out (unsafe-fx+ offset (unsafe-fx* 4 i)) v))
        (box-line* radius start end get-val set-val)))))

(define (box-v in out radius w h iterations)
  (for/async ([col (in-range w)])
    (for ([iter (in-range iterations)])
      (let ([start 0]
            [end h]
            [in (if (even? iter) in out)]
            [out (if (even? iter) out in)])
        (define-syntax-rule (get-val i offset)
          (bytes-ref in (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset)))
        (define-syntax-rule (set-val i offset v)
          (bytes-set! out (unsafe-fx+ (unsafe-fx* 4 (unsafe-fx+ (unsafe-fx* w i) col)) offset) v))
        (box-line* radius start end get-val set-val)))))

(define (ceil/e x) (inexact->exact (ceiling x)))

;; ----

;; used for benchmarking to force effectively lazy dc pict constructor
(define (p->bmp p)
  (let* ([bmp (make-object bitmap% (ceil/e (pict-width p)) (ceil/e (pict-height p)))]
         [bdc (new bitmap-dc% (bitmap bmp))])
    (draw-pict p bdc 0 0)
    bmp))

;; ============================================================
;; Boxes with Keynote-style shadows

(define (shadow-frame #:background-color [background-color "white"]
                      #:frame-color [frame-color "gray"]
                      #:frame-line-width [frame-line-width 0]
                      #:shadow-side-length [s-side-len 4.0]
                      #:shadow-top-y-offset [s-top-dy 10.0]
                      #:shadow-bottom-y-offset [s-bot-dy 4.0]
                      #:shadow-descent [s-desc 40.0]
                      #:shadow-alpha-factor [s-alpha 3/4]
                      #:blur [blur-radius 20]
                      #:margin [margin-len 20]
                      #:sep [sep 5]
                      . picts)
  ;; shadow-alpha-factor:
  ;;   - default 3/4 good for a heavy shadow, if blur is enabled
  ;;   - about 1/4 or 1/5 good for light shadow w/o blur
  (let* ([pict (apply vl-append sep picts)]
         [pict (inset pict margin-len)]
         [w (pict-width pict)]
         [h (pict-height pict)]
         [main-box (frame (colorize (filled-rectangle w h) background-color)
                          #:color frame-color #:line-width frame-line-width)]
         [w* (+ w s-side-len s-side-len)]
         [shadow (arch w* w* (+ h (- s-bot-dy s-top-dy)) s-desc)]
         [shadow (brush/linear-gradient
                  shadow
                  (mk-shadow-grad-stops w* s-side-len s-alpha))]
         [shadow
          (cond [(zero? blur-radius) shadow]
                [#t ;; use-smart-blur?
                 (smart-blur shadow w h blur-radius
                             s-side-len s-top-dy s-bot-dy s-desc)]
                [else (blur shadow blur-radius)])]
         [result
          (pin-under (cc-superimpose main-box pict)
                     (- s-side-len) s-top-dy
                     shadow)]
         [result
          (inset result s-side-len 0
                 s-side-len (+ s-desc (- s-top-dy s-bot-dy)))])
    (inset result blur-radius)))

;; smart-blur: blur only visible edges
(define (smart-blur shadow w0 h0 blur-radius s-side-len s-top-dy s-bot-dy s-desc)
  (define (blur-part p x1 y1 x2 y2 lpad tpad rpad bpad)
    (let* ([p (viewport p (- x1 lpad) (- y1 tpad) (+ x2 rpad) (+ y2 bpad))]
           [p (blur p blur-radius #:pre-inset? #f)]
           [p (clip (inset p (- lpad) (- tpad) (- rpad) (- bpad)))])
      p))
  (define (viewport p x1 y1 x2 y2)
    (clip (pin-over (blank (- x2 x1) (- y2 y1)) (- x1) (- y1) p)))
  (let* ([shadow* (inset shadow blur-radius)]
         [w* (pict-width shadow*)]
         [h* (pict-height shadow*)]
         [BR blur-radius]

         [yTopBot (+ BR (- s-top-dy))]
         [yMidBot (+ yTopBot h0)]
         [xLeftRight (+ BR s-side-len)]

         [top-part
          (blur-part shadow*
                     0 0 w* yTopBot
                     0 0 0 BR)]
         [left-part
          (blur-part shadow*
                     0 yTopBot xLeftRight yMidBot
                     0 BR BR BR)]
         [right-part
          (blur-part shadow*
                     (- w* xLeftRight) yTopBot w* yMidBot
                     BR BR 0 BR)]
         [bot-part
          (blur-part shadow*
                     0 yMidBot w* h*
                     0 BR 0 0)]

         [result (blank w* h*)]
         [result (pin-over result 0 0 top-part)]
         [result (pin-over result 0 yTopBot left-part)]
         [result (pin-over result (- w* xLeftRight) yTopBot right-part)]
         [result (pin-over result 0 yMidBot bot-part)])
    (inset result (- blur-radius))))

(define (mk-shadow-grad-stops w s-side-len s-alpha)
  (let* ([epsA (/ s-side-len w)]
         [epsZ (- 1.0 epsA)]
         [alphaA (max 0 (min 1 (* s-alpha 0.16)))]
         [alphaB (max 0 (min 1 (* s-alpha 0.25)))]
         [alphaC (max 0 (min 1 (* s-alpha 1.00)))])
    (list (list 0.00 (make-object color% 0 0 0 alphaA))
          (list epsA (make-object color% 0 0 0 alphaB))
          (list 0.25 (make-object color% 0 0 0 alphaC))
          (list 0.75 (make-object color% 0 0 0 alphaC))
          (list epsZ (make-object color% 0 0 0 alphaB))
          (list 1.00 (make-object color% 0 0 0 alphaA)))))

;; ----

(define (arch outer-w inner-w solid-h leg-h)
  (dc (lambda (dc X Y)
        (draw-arch dc X Y outer-w inner-w solid-h leg-h))
      outer-w (+ solid-h leg-h)))

(define (draw-arch dc X Y outer-w inner-w solid-h leg-h)
  (cond [(zero? leg-h)
         (send dc draw-rectangle X Y outer-w solid-h)]
        [else
         (let ([path (new dc-path%)])
           (dc-path-arch path X Y outer-w inner-w solid-h leg-h)
           (send dc draw-path path))]))

;; closes path's current sub-path and draws the outline of an arch, clockwise
;; requires leg-h != 0
(define (dc-path-arch path X Y outer-w inner-w solid-h leg-h)
  (let* ([xA X]
         [xB (+ X outer-w)]
         [xMid (/ (+ xA xB) 2.0)]
         [ySolidEnd (+ Y solid-h)]
         [yEnd (+ Y solid-h leg-h)]
         [hdx (/ (- outer-w inner-w) 2.0)]
         [xAi (+ xA hdx)]
         [xBi (- xB hdx)]
         [radius (+ (/ leg-h 2) (/ (sqr inner-w) 8 leg-h))]
         [diameter (+ radius radius)]
         [theta (asin (/ (- radius leg-h) radius))])
    (send* path
      (move-to xA Y)
      (line-to xB Y)
      (line-to xB ySolidEnd)
      (line-to xB yEnd)
      (line-to xBi yEnd)
      (arc (- xMid radius) ySolidEnd
           diameter diameter
           theta
           (- pi theta))
      ;; ends at *roughly* xAi yEnd
      (line-to xAi yEnd)
      (line-to xA yEnd)
      (line-to xA ySolidEnd)
      (line-to xA Y))))

;; ====

(define no-pen (make-object pen% "BLACK" 1 'transparent))

(define (brush/linear-gradient p stops)
  (let* ([drawer (make-pict-drawer p)]
         [w (pict-width p)]
         [h (pict-height p)])
    (dc (lambda (dc X Y)
          (let* ([grad
                  (new linear-gradient%
                       ;; Apparently gradient handles scaling,
                       ;; rotation, etc automatically (???)
                       (x0 X) (y0 Y) (x1 (+ X w)) (y1 Y)
                       (stops stops))]
                 [new-brush (new brush% (gradient grad))]
                 [old-pen (send dc get-pen)]
                 [old-brush (send dc get-brush)])
            (send* dc
              (set-pen no-pen)
              (set-brush new-brush))
            (drawer dc X Y)
            (send* dc
              (set-pen old-pen)
              (set-brush old-brush))))
        w h)))

#|
;; FIXME:
;;   (arch ....) by itself draws outline
;;   (colorize (arch ....) "red") draws filled (no outline, or same color)

Problem: picts, colorize, etc not designed to inherit brush. See
texpict/utils: filled-rectangle, eg, makes new brush from pen color;
rectangle makes new transparent brush.

|#

;; ----

;; provided by unstable/gui/pict, for backwards compatibility
(module+ unstable
  (provide/contract
   [blur-bitmap!
    (->* ((is-a?/c bitmap%) exact-nonnegative-integer?)
         (exact-nonnegative-integer?)
         void?)]
   [arch
    (-> real? real? real? real?
        pict?)])
  (define (blur-bitmap! bmp hbr [vbr hbr])
    (blur! bmp hbr vbr)))
