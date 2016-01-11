#lang racket
(require pict rackunit
         (for-syntax syntax/parse
                     racket/sequence)
         pict/shadow
         racket/random
         pict/code
         pict/conditional
         pict/balloon
         racket/draw racket/class)

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

(define-check (check-pict=?/msg actual expected msg)
  (unless (equal? (->bitmap actual) (->bitmap expected))
    (fail-check msg)))
(define-syntax check-pict=?
  (syntax-parser
    [(_ actual expected) #'(check-pict=?/msg actual expected "")]
    [(_ actual expected msg) #'(check-pict=?/msg actual expected msg)]))

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
   (check-pict=?/msg p (freeze p) (format "~a" l))))

(test-case
 "scale-to-fit"
 (define p (rectangle 10 20))
 (check-pict=? (scale-to-fit p p) p)
 (check-pict=? (scale-to-fit p (scale p 2)) (scale p 2))
 (check-pict=? (scale-to-fit p 40 40) (scale p 2))
 (check-pict=? (scale-to-fit p 40 40 #:mode 'inset)
               (cc-superimpose (blank 40 40)
                               (scale p 2)))
 (check-pict=? (scale-to-fit p 40 40 #:mode 'distort) (scale p 4 2)))


;; check whether the new implementation of shapes (with borders and colors)
;; are equivalent to the old ones (for the feature subset of the old one)

(define (old-filled-rectangle w h #:draw-border? [draw-border? #t])
  (dc
   (lambda (dc x y)
     (let ([b (send dc get-brush)]
           [p (send dc get-pen)])
       (send dc set-brush (send the-brush-list find-or-create-brush
                                (send p get-color)
                                'solid))
       (unless draw-border?
         (send dc set-pen "black" 1 'transparent))
       (send dc draw-rectangle x y w h)
       (send dc set-brush b)
       (send dc set-pen p)))
   w
   h))

(define (old-rectangle w h)
  (dc
   (lambda (dc x y)
     (let ([b (send dc get-brush)])
       (send dc set-brush (send the-brush-list find-or-create-brush
                                "white" 'transparent))
       (send dc draw-rectangle x y w h)
       (send dc set-brush b)))
   w
   h))

(define (old-rounded-rectangle w h [corner-radius -0.25] #:angle [angle 0])
  (let ([dc-path (new dc-path%)])
    (send dc-path rounded-rectangle 0 0 w h corner-radius)
    (send dc-path rotate angle)
    (let-values ([(x y w h) (send dc-path get-bounding-box)])
      (dc (λ (dc dx dy)
            (let ([brush (send dc get-brush)])
              (send dc set-brush (send the-brush-list find-or-create-brush
                                       "white" 'transparent))
              (send dc draw-path dc-path (- dx x) (- dy y))
              (send dc set-brush brush)))
          w
          h))))

(define (old-filled-rounded-rectangle w h [corner-radius -0.25] #:angle [angle 0] #:draw-border? [draw-border? #t])
  (let ([dc-path (new dc-path%)])
    (send dc-path rounded-rectangle 0 0 w h corner-radius)
    (send dc-path rotate angle)
    (let-values ([(x y w h) (send dc-path get-bounding-box)])
      (dc (λ (dc dx dy) 
            (let ([brush (send dc get-brush)]
                  [pen (send dc get-pen)])
              (send dc set-brush (send the-brush-list find-or-create-brush
                                       (send (send dc get-pen) get-color)
                                       'solid))
              (unless draw-border?
                (send dc set-pen "black" 1 'transparent))
              (send dc draw-path dc-path (- dx x) (- dy y))
              (send dc set-brush brush)
              (send dc set-pen pen)))
          w
          h))))

(define (old-circle size) (ellipse size size))

(define (old-ellipse width height)
  (dc (lambda (dc x y)
        (let ([b (send dc get-brush)])
          (send dc set-brush (send the-brush-list find-or-create-brush
                                   "white" 'transparent))
          (send dc draw-ellipse x y width height)
          (send dc set-brush b)))
      width height))

(define (old-disk size #:draw-border? [draw-border? #t])
  (filled-ellipse size size #:draw-border? draw-border?))

(define (old-filled-ellipse width height #:draw-border? [draw-border? #t])
  (dc (lambda (dc x y)
        (define b (send dc get-brush))
        (define p (send dc get-pen))
        (send dc set-brush (send the-brush-list find-or-create-brush
                                 (send (send dc get-pen) get-color)
                                 'solid))
        (unless draw-border?
          (send dc set-pen "black" 1 'transparent))
        (send dc draw-ellipse x y width height)
        (send dc set-brush b)
        (send dc set-pen p))
      width height))

(define (random-boolean) (> (random) 0.5))
(define (generate-shapes depth)
  (define r (random (if (= depth 0) 8 15)))
  (case r
    [(0) (let ([w (random 10)]
               [h (random 10)])
           (values (old-rectangle w h)
                   (rectangle w h)
                   `(rectangle ,w ,h)))]
    [(1) (let ([w (random 10)]
               [h (random 10)]
               [border? (random-boolean)])
           (values (old-filled-rectangle w h #:draw-border? border?)
                   (filled-rectangle w h #:draw-border? border?)
                   `(filled-rectangle ,w ,h #:draw-border? ,border?)))]
    [(2) (let ([w (random 10)]
               [h (random 10)]
               [corner (- (random) 0.5)]
               [angle (* (- (random) 0.5) 2 pi)])
           (values (old-rounded-rectangle w h corner #:angle angle)
                   (rounded-rectangle w h corner #:angle angle)
                   `(rounded-rectangle ,w ,h ,corner #:angle ,angle)))]
    [(3) (let ([w (random 10)]
               [h (random 10)]
               [border? (random-boolean)]
               [corner (- (random) 0.5)]
               [angle (* (- (random) 0.5) 2 pi)])
           (values (old-filled-rounded-rectangle w h corner
                                                 #:angle angle
                                                 #:draw-border? border?)
                   (filled-rounded-rectangle w h corner
                                             #:angle angle
                                             #:draw-border? border?)
                   `(filled-rounded-rectangle ,w ,h ,corner
                                              #:angle ,angle
                                              #:draw-border? ,border?)))]
    [(4) (let ([r (random 10)])
           (values (old-circle r)
                   (circle r)
                   `(circle ,r)))]
    [(5) (let ([w (random 10)]
               [h (random 10)])
           (values (old-ellipse w h)
                   (ellipse w h)
                   `(ellipse ,w ,h)))]
    [(6) (let ([r (random 10)]
               [border? (random-boolean)])
           (values (old-disk r #:draw-border? border?)
                   (disk r #:draw-border? border?)
                   `(disk ,r #:draw-border? ,border?)))]
    [(7) (let ([w (random 10)]
               [h (random 10)]
               [border? (random-boolean)])
           (values (old-filled-ellipse w h #:draw-border? border?)
                   (filled-ellipse w h #:draw-border? border?)
                   `(filled-ellipse ,w ,h #:draw-border? ,border?)))]
    [(8) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                      [(old2 new2 t2) (generate-shapes (sub1 depth))])
           (values (cc-superimpose old1 old2)
                   (cc-superimpose new1 new2)
                   `(cc-superimpose ,t1 ,t2)))]
    [(9) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                      [(old2 new2 t2) (generate-shapes (sub1 depth))])
           (values (ht-append old1 old2)
                   (ht-append new1 new2)
                   `(ht-append ,t1 ,t2)))]
    [(10) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                       [(old2 new2 t2) (generate-shapes (sub1 depth))])
            (values (hc-append old1 old2)
                    (hc-append new1 new2)
                    `(hc-append ,t1 ,t2)))]
    [(11) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                       [(old2 new2 t2) (generate-shapes (sub1 depth))])
            (values (hb-append old1 old2)
                    (hb-append new1 new2)
                    `(hb-append ,t1 ,t2)))]
    [(12) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                       [(old2 new2 t2) (generate-shapes (sub1 depth))])
            (values (vl-append old1 old2)
                    (vl-append new1 new2)
                    `(vl-append ,t1 ,t2)))]
    [(13) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                       [(old2 new2 t2) (generate-shapes (sub1 depth))])
            (values (vc-append old1 old2)
                    (vc-append new1 new2)
                    `(vc-append ,t1 ,t2)))]
    [(14) (let-values ([(old1 new1 t1) (generate-shapes (sub1 depth))]
                       [(old2 new2 t2) (generate-shapes (sub1 depth))])
            (values (vr-append old1 old2)
                    (vr-append new1 new2)
                    `(vr-append t1 t2)))]))

(test-case
 "old and new shapes"
 (for ([i 1000])
   (define-values (old new trace) (generate-shapes 4))
   (check-pict=?/msg old new (format "~a" trace))))

;; a few that caused issues with previous version of the new implementation
(check-pict=? (ellipse 7 3) (old-ellipse 7 3))
(check-pict=? (hc-append (hc-append (circle 3) (ellipse 0 0))
                         (rectangle 0 9))
              (hc-append (hc-append (old-circle 3) (old-ellipse 0 0))
                         (old-rectangle 0 9)))
(check-pict=? (rectangle 1 3) (old-rectangle 1 3))
(check-pict=? (ht-append (ellipse 3 1)
                         (hb-append (filled-rectangle 8 2 #:draw-border? #t)
                                    (ellipse 5 0)))
              (ht-append (old-ellipse 3 1)
                         (hb-append (old-filled-rectangle 8 2 #:draw-border? #t)
                                    (old-ellipse 5 0))))
(check-pict=? (rectangle 0 3) (old-rectangle 0 3))

;; check that loading pict-convertables as picts doesn't cause odd behavior

(require pict/convert)
(struct wrap (pict)
  #:property prop:pict-convertible
  (lambda (x) (pict-convert (wrap-pict x))))

(test-case "check pict-post"
  (local-require (submod pict/private/pict convertible))
  (let ([x (wrap (text "xx"))])
    (check-true (pict-path-element=? x (pict-convert x)))))

(test-case "find-XX with wrapping tests"
  (check-not-exn
   (let ([x (wrap (text "sefse"))])
     (thunk (lt-find (pict-convert x) x))))
  (check-not-exn
   (let ([x (wrap (text "sefse"))])
     (thunk (lt-find x x)))))

(define-syntax gen-wrapping-case
  (syntax-parser
    [(_ gen:id cut:id s:id e:expr cls ...)
     (with-syntax ([(c ...) (for/list ([c (syntax->list #'(cls ...))]
                                       [i (in-naturals)])
                              (transform-clause c i #'gen #'cut))])
       #`(let ([s #,(length (syntax->list #'(c ...)))])
           (let ([n e])
             (case n c ... [else (error 'cs "unknown case ~a" n)]))))]))

(define-for-syntax (transform-clause stx i gen cut)
  (with-syntax ([n i])
    (syntax-parse stx
      [[#:skip b]
       #`[(n) b]]
      [(m:id b:expr ...)
       (with-syntax ([(i ...) (generate-temporaries (syntax->list #'(b ...)))])
         (define num
           (for/sum ([x (in-syntax #'(b ...))])
             (syntax-parse x
               [(g)
                #:when (free-identifier=? #'g gen)
                1]
               [_ 0])))
         #`[(n)
            (parameterize ([#,cut #,num])
              (define i (call-with-values (lambda () b) list)) ...
              (with-handlers ([void (lambda (ex) (displayln `(m ,(last i) ...)) (raise ex))])
                (values
                 (m (first i) ...)
                 (wrap (m (if (or (null? (rest i))
                                  (null? (rest (rest i))))
                              (first i)
                              (second i))
                          ...))
                 `(m ,(last i) ...))))])])))

(require (prefix-in htdp: 2htdp/image))
(define (generate-pict/wrap)
  (define (random-number-in [a #f])
    (if (not a)
        (random)
        (case (random-ref  '(integer float exact))
          [(integer) (random a)]
          [(float) (+ (random (sub1 a)) (random))]
          [(exact) (/ (random a) (random 1 a))])))
  (define-values (l p m)
    (let loop ([fuel 20])
      (define cut (make-parameter 1))
      (define (gen [fuel-cut (cut)])
        (loop (floor (/ (sub1 fuel) fuel-cut))))
      (gen-wrapping-case gen cut count
       (if (= fuel 0) (random 6) (random count))
       (text "sefsefse")
       (rectangle (add1 (random-number-in 10)) (add1 (random-number-in 10)))
       (arrow (add1 (random-number-in 10)) (add1 (random-number-in 10)))
       (jack-o-lantern (add1 (random-number-in 10)))
       (standard-fish 100 50)
       (htdp:triangle (add1 (random-number-in 40)) "solid" "tan")
       (thermometer)
       (frame (gen))
       (cc-superimpose (gen) (gen))
       (vl-append (gen) (gen))
       (hbl-append (gen) (gen))
       (rb-superimpose (gen) (gen))
       (panorama (gen))
       (scale (gen) (add1 (random-number-in)))
       (inset (gen) (random-number-in 10) (random-number-in 10) (random-number-in 10) (random-number-in 10))
       (baseless (gen))
       (scale-to-fit (gen) (add1 (random-number-in 100)) (add1 (random-number-in 100)))
       (rotate (gen) (* 1/2 pi (random-number-in 4)))
       (ghost (gen))
       (linewidth (random-number-in 10) (gen))
       (linestyle (first (shuffle (list'transparent 'solid 'xor 'hilite
                                                     'dot 'long-dash 'short-dash 'dot-dash
                                                     'xor-dot 'xor-long-dash 'xor-short-dash
                                                     'xor-dot-dash)))
                   (gen))
        (colorize (gen) (list (random 254) (random 254) (random 254)))
        (cellophane (gen) (random-number-in))
        (clip (gen))
        (clip-descent (gen))
        (clip-ascent (gen))
        (freeze (gen))
        (blur (gen) (add1 (random-number-in 10)))
        (shadow-frame (gen))
        (pict-if (> .5 (random-number-in))
                 (gen)
                 (gen))
        (show (gen) (> .5 (random-number-in)))
        (hyperlinkize (gen))
        (pin-over (gen)
                  (random-number-in 10)
                  (random-number-in 10)
                  (gen))
        (table 2
               (let-values ([(l1 r1 m1) (gen 4)]
                            [(l2 r2 m2) (gen 4)]
                            [(l3 r3 m3) (gen 4)]
                            [(l4 r4 m4) (gen 4)])
                 (values (list l1 l2 l3 l4)
                         (list r1 r2 r3 r4)
                         `(list ,m1 ,m2 ,m3 ,m4)))
               cc-superimpose
               cc-superimpose
               (random-number-in 5)
               (random-number-in 5))
       [#:skip
        (let-values ([(l w m) (gen)])
          (with-handlers ([void (lambda (e)
                                  (displayln `(code (+ 1 #,m)))
                                  (raise e))])
            (values (code (+ 1 #,l))
                    (wrap (code (+ 1 #,w)))
                    `(code (+ 1 #,m)))))]
       (code-align (gen))
       (pip-wrap-balloon (gen)
                         (first (shuffle (list 'n 's 'e 'w 'ne 'se 'sw 'nw)))
                         (random-number-in 10)
                         (random-number-in 10))
       (fade-pict (random-number-in) (gen) (gen))
       [#:skip
        (let*-values ([(l1 r1 m1) (gen)]
                      [(l2 r2 m2) (gen)]
                      [(l3) (hbl-append l1 l2)]
                      [(r3) (hbl-append r1 r2)]
                      [(m3) `(hbl-append ,m1 ,m2)])
          (with-handlers ([void (lambda (e) (displayln `(use-last ,m3 (pict-last ,m3))) (raise e))])
            (values
             (use-last l3 (pict-last l3))
             (use-last r3 (pict-last r3))
             `(use-last ,m3 (pict-last ,m3)))))]
       [#:skip
        (let-values ([(l1 r1 m1) (gen 3)]
                     [(l2 r2 m2) (gen 3)]
                     [(l3 r3 m3) (gen 3)]
                     [(f) (random)])
          (with-handlers ([void (lambda (e)
                                  (displayln `(slide-pict ,m3
                                                          (hbl-append ,m1 ,m2)
                                                          ,m1
                                                          ,m2
                                                          ,f))
                                  (raise e))])
            (define (mk i l r)
              (slide-pict i (hbl-append l r) l r f))
            (values (mk l3 l1 l2)
                    (mk r3 r1 r2)
                    `(slide-pict ,m3
                                 (hbl-append ,m1 m2)
                                 m1
                                 m2
                                 ,f))))])))
  (values l p m))


(require rackunit/text-ui)
(run-tests
 (make-test-suite
  "auto pict conversion"
  (for/list ([i 1000])
    (test-suite ""
      (check-not-exn
       (thunk
        (define-values (l r m) (generate-pict/wrap))
        (check-pict=? l r (~a m))))))))

;;;; here are tests that exibit bugs found by the random testing in the past

;; this originally failed due to a floating point error in dash-line
(check-not-exn (lambda () (frame (rectangle 1519/25 48.0))))
