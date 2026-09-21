#lang racket/base
(require "../main.rkt"
         racket/match
         racket/class)

(provide (struct-out tree-layout)
         (struct-out tree-edge)
         binary-tree-layout?
         _tree-layout?
         _tree-layout
         _tree-edge
         default-node-pict
         compute-spacing
         uniquify-picts
         transform-tree-pict)

;; values of this struct leak outside, so it cannot be transparent
(struct tree-layout (pict children))
(struct tree-edge (child edge-color edge-width edge-style))

(define _tree-layout
  (let ([constructor tree-layout])
    (define (tree-layout #:pict [node-pict #f]
                         . children)
      (constructor (or node-pict default-node-pict)
                   (for/list ([child (in-list children)])
                     (cond
                       [(tree-edge? child) child]
                       [(not child) child]
                       [else (_tree-edge child)]))))
    tree-layout))

(define _tree-layout?
  (let ([predicate tree-layout?])
    (define (tree-layout? v)
      (or (not v) (predicate v)))
    tree-layout?))

(define default-node-pict
  (cc-superimpose
   (disk 16 #:draw-border? #f)
   (colorize (disk 12 #:draw-border? #f) "white")))

(define _tree-edge
  (let ([constructor tree-edge])
    (define (tree-edge child
                       #:edge-color [edge-color "gray"]
                       #:edge-width [edge-width 'unspecified]
                       #:edge-style [edge-style 'unspecified])
      (constructor child edge-color edge-width edge-style))
    tree-edge))

(define (binary-tree-layout? t)
  (match t
    [#f #t]
    [(tree-layout pict (list left right))
     (and (binary-tree-edge? left)
          (binary-tree-edge? right))]
    [else #f]))

(define (binary-tree-edge? e)
  (match e
    [(tree-edge t _ _ _) (binary-tree-layout? t)]
    [#f #t]))

(define (compute-spacing t given-x-spacing given-y-spacing)
  (cond
    [(and given-x-spacing given-y-spacing)
     (values given-x-spacing given-y-spacing)]
    [else
     (define x-spacing 0)
     (define y-spacing 0)
     
     (let loop ([t t])
       (match t
         [#f (void)]
         [(tree-layout pict (list children ...))
          (set! x-spacing (max (pict-width pict) x-spacing))
          (set! y-spacing (max (pict-height pict) y-spacing))
          (for ([edge (in-list children)])
            (match edge
              [#f (void)]
              [(tree-edge child edge-color _ _)
               (loop child)]))]))
     
     (values (or given-x-spacing x-spacing)
             (or given-y-spacing y-spacing))]))

;; If `transform` is `#f`, then we'll reuse the layout pict and just place
;; edges on top of it.
(define (transform-tree-pict t layout-pict transform node-connection-style)
  (define-values (l-amt t-amt r-amt b-amt) (values #f #f #f #f))
  (define (update-bb! x y)
    (set! l-amt (if l-amt (min l-amt x) x))
    (set! r-amt (if r-amt (max r-amt x) x))
    (set! t-amt (if t-amt (min t-amt y) y))
    (set! b-amt (if b-amt (max b-amt y) y)))
  (define w (pict-width layout-pict))
  (define h (pict-height layout-pict))
  (when transform
    (for ([x `(0 ,w ,w 0)]
          [y `(0 0 ,h ,h)])
      (define-values (tx ty) (transform x y))
      (update-bb! tx ty)))
  (define (place-node main pict)
    (define-values (x y) (lt-find layout-pict pict))
    (define-values (tx ty) (transform x y))
    (pin-over main tx ty pict))
  (define final-pict
   (let loop ([t t]
              [main (if transform (blank) layout-pict)]
              [parent-pict #f])
     (match t
       [#f main]
       [(tree-edge child edge-color edge-width edge-style)
        (define child-pict (tree-layout-pict child))
        (let* ([main (loop child main #f)]
               [main (connect-child-to-parent main
                                              parent-pict
                                              child-pict
                                              #:line-width (and (not (unspecified? edge-width)) edge-width)
                                              #:style (and (not (unspecified? edge-style)) edge-style)
                                              #:color edge-color
                                              #:node-connection-style node-connection-style)])
          main)]
       [(tree-layout pict children)
        (for/fold ([main (if transform (place-node main pict) main)])
                  ([child (in-list children)])
          (loop child main pict))])))
   (if transform (inset final-pict l-amt t-amt r-amt b-amt) final-pict))

(define (connect-child-to-parent main
                                 parent-pict
                                 child-pict
                                 #:line-width line-width
                                 #:style style
                                 #:color color
                                 #:node-connection-style node-connection-style)
  (match node-connection-style
    ['direct
     (pin-line main
               parent-pict cc-find
               child-pict cc-find
               #:line-width line-width
               #:style style
               #:color color
               #:under? #t)]
    ['orthogonal
     (define-values (px py) (cc-find main parent-pict))
     (define-values (cx cy) (cc-find main child-pict))
     (define cpy2 (/ (+ py cy) 2))
     (define points (list (cons px py)
                          (cons px cpy2)
                          (cons cx cpy2)
                          (cons cx cy)))
     (define line-drawing-pict
       (dc
        (λ (dc dx dy)
          (for ([p1 (in-list points)]
                [p2 (in-list (cdr points))])
            (send dc draw-line
                  (+ dx (car p1)) (+ dy (cdr p1))
                  (+ dx (car p2)) (+ dy (cdr p2)))))
        (pict-width main)
        (pict-height main)
        (pict-ascent main)
        (pict-descent main)))
     (when line-width
       (set! line-drawing-pict
             (linewidth line-width line-drawing-pict)))
     (when style
       (set! line-drawing-pict
             (linestyle style line-drawing-pict)))
     (when color
       (set! line-drawing-pict (colorize line-drawing-pict color)))
     (cc-superimpose
      line-drawing-pict
      main)]))

(define (uniquify-picts t)
  (let loop ([t t])
    (match t
      [#f #f]
      [(tree-layout pict children)
       (tree-layout (unique-pict pict) (map loop children))]
      [(tree-edge child c w s)
       (tree-edge (loop child) c w s)])))

(define (unique-pict pict)
  (cc-superimpose pict))

(define (unspecified? x)
  (eq? x 'unspecified))
