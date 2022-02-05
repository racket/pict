#lang racket/base
(require racket/match
         "../main.rkt"
         "layout.rkt")
(provide naive-layered)
(define (naive-layered t #:x-spacing [given-x-spacing #f]
                         #:y-spacing [given-y-spacing #f]
                         #:invert? [invert? #f])
  (define-values (x-space y-space) (compute-spacing t given-x-spacing given-y-spacing))
  (define-values (level-space child-space level-append child-append)
    (if invert?
        (values x-space y-space hc-append vl-append)
        (values y-space x-space vc-append ht-append)))

  (define root+tree-pair
    (let loop ([t t])
      (match t
        [#f (cons #f (blank))]
        [(tree-layout pict children)
         (cond
           [(andmap not children)
            (define this-root (ghost (launder pict)))
            (cons this-root (cc-superimpose this-root pict))]
           [else
            (define children-pairs
              (for/list ([child (in-list children)])
                (match child
                  [#f 
                   (define b (blank))
                   (cons b b)]
                  [(tree-edge child color _ _)
                   (loop child)])))
            (define this-root (launder (ghost pict)))
            (define children-roots (map car children-pairs))
            (define children-trees (map cdr children-pairs))
            (let loop ([main (place-parent-over-children
                              (cc-superimpose this-root pict)
                              children-roots
                              (level-append
                               level-space
                               (ghost (launder pict))
                               (apply child-append child-space children-trees))
                              invert?)]
                       [children-roots children-roots]
                       [tree-edges children])
              (cond
                [(null? children-roots) (cons this-root main)]
                [else 
                 (define child-root (car children-roots))
                 (define this-tree-edge (car tree-edges))
                 (match this-tree-edge
                   [#f (loop main (cdr children-roots) (cdr tree-edges))]
                   [(tree-edge child edge-color edge-width edge-style)
                    (define *w/line
                      (colorize
                       (launder
                        (pin-line (ghost main)
                                  this-root cc-find
                                  child-root cc-find))
                       edge-color))
                    (define w/line
                      (let ([w/width
                             (if (eq? edge-width 'unspecified)
                                 *w/line
                                 (linewidth edge-width *w/line))])
                        (if (eq? edge-style 'unspecified)
                            w/width
                            (linestyle edge-style w/width))))
                    (loop (cc-superimpose w/line main)
                          (cdr children-roots)
                          (cdr tree-edges))])]))])])))
  
  (cdr root+tree-pair))

(define (place-parent-over-children parent-root children-roots main invert?)
  (define-values (size from-find to-find)
    (if invert?
        (values pict-height ct-find cb-find)
        (values pict-width lc-find rc-find)))

  (define coord-min (size main))
  (define coord-max 0)
  (for ([child-root (in-list children-roots)])
    (when child-root
      (define-values (x-min y-min) (from-find main child-root))
      (define-values (x-max y-max) (to-find main child-root))

      (set! coord-min (min (if invert? y-min x-min) coord-min))
      (set! coord-max (max (if invert? y-max x-max) coord-max))))

  (define adjustment (- (/ (+ coord-min coord-max) 2) (/ (size parent-root) 2)))
  (pin-over main
            (if invert? 0 adjustment)
            (if invert? adjustment 0)
            parent-root))
                             

(module+ test 
  (require rackunit)
  (check-pred pict? (naive-layered #f))
  (check-pred pict? (naive-layered (_tree-layout)))
  (check-pred pict? (naive-layered (_tree-layout
                                    (_tree-layout)
                                    (_tree-layout))))
  (check-pred pict? (naive-layered (_tree-layout
                                    (_tree-layout)
                                    (_tree-layout)
                                    (_tree-layout
                                     (_tree-layout)
                                     (_tree-layout)
                                     (_tree-layout
                                      (_tree-layout)
                                      (_tree-layout)))))))

(module+ main
  (define (complete n)
    (cond
      [(= n 0) #f]
      [else
       (define t (complete (- n 1)))
       (apply _tree-layout (build-list n (λ (_) t)))]))
  
  (naive-layered (complete 4))
  (define right-subtree-with-long-left-chain
    (_tree-layout
     (_tree-layout
      (_tree-layout #f #f)
      (_tree-layout
       (_tree-layout #f #f)
       #f))
     (_tree-layout
      (_tree-layout
       (_tree-layout
        (_tree-layout 
         (_tree-layout #f #f)
         #f)
        #f)
       #f)
      #f)))
  (naive-layered right-subtree-with-long-left-chain))
