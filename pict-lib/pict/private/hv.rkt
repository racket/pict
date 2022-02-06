#lang racket/base
(require "../main.rkt"
         "layout.rkt"
         racket/match)

#|

A note on optimal area algorithms for upward drawing of binary trees
P. Crescenzi, G. Di Battista, and A. Piperno
Computational Geometry, Theory and Applications 2 (1992)

|#
(provide hv-alternating)
(define (hv-alternating t
                        #:x-spacing [given-x-spacing #f]
                        #:y-spacing [given-y-spacing #f]
                        #:transform [transform #f])
  (define-values (x-size y-size) (compute-spacing t #f #f))
  (define x-spacing (or given-x-spacing (* x-size 1.5)))
  (define y-spacing (or given-y-spacing (* y-size 1.5)))
  (define t-unique (uniquify-picts t))
  (define main
    (inset
     (let loop ([t t-unique]
                [l #t])
       (match t
         [#f (blank)]
         [(tree-layout pict (list left right))
          (define left-t (and (tree-edge? left) (tree-edge-child left)))
          (define right-t (and (tree-edge? right) (tree-edge-child right)))
          (cond
            [(and (not left-t) (not right-t))
             (dot-ize pict)]
            [(not left-t)
             (empty-left (dot-ize pict) x-spacing (loop right-t (not l)))]
            [(not right-t)
             (empty-right (dot-ize pict) y-spacing (loop left-t (not l)))]
            [else
             (define left-p (loop left-t (not l)))
             (define right-p (loop right-t (not l)))
             (define main
               ((if l left-right top-bottom)
                x-spacing y-spacing
                left-p right-p))
             (pin-over
              main
              (- (/ (pict-width pict) 2))
              (- (/ (pict-height pict) 2))
              pict)])]))
     (/ x-size 2)
     (/ y-size 2)))

  (transform-tree-pict t-unique main transform))

(define (dot-ize p)
  (define b (blank))
  (refocus (cc-superimpose b p) b))

(define (left-right hgap vgap left right)
  (ht-append
   hgap
   (vl-append (blank 0 vgap) left)
   right))

(define (top-bottom hgap vgap left right)
  (vl-append 
   vgap
   (ht-append (blank hgap 0) left)
   right))

(define (empty-left pict hgap sub-tree-p)
  (ht-append hgap pict sub-tree-p))

(define (empty-right pict vgap sub-tree-p)
  (vl-append vgap pict sub-tree-p))

(module+ test
  (require rackunit)
  
  (check-pred pict? 
              (hv-alternating 
               (let* ([p1 (_tree-layout #f #f)]
                      [p2 (_tree-layout p1 p1)]
                      [p3 (_tree-layout p2 p2)]
                      [p4 (_tree-layout p3 p3)])
                 (_tree-layout p4 p4))))
  (check-pred 
   pict?
   (hv-alternating (_tree-layout (_tree-layout #f #f) #f)))
  (check-pred
   pict?
   (hv-alternating (_tree-layout #f (_tree-layout #f (_tree-layout #f #f)))))
  (check-pred pict? (hv-alternating #f)))

(module+ main
  (define (complete n)
    (cond
      [(= n 0) #f]
      [else
       (define t (complete (- n 1)))
       (_tree-layout t t)]))
  
  ;; an example from the paper
  (hv-alternating (complete 4)))

