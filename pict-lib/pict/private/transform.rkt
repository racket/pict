#lang racket/base
(require "pict.rkt" racket/match racket/list)
(provide get-child-transformation)

;; get a matrix suitable for the transform method of
;; dc<%> which shifts one into the drawing coordinates
;; of the pict given by the pict path pp.
(define (get-child-transformation p pp)
  (convert-affine-form
   (uninvert-coordinartes
    p (last (flatten pp))
    (cond
      [(pict-convertible? pp)
       (if (pict-path-element=? p pp)
           (vector 1 0 0
                   0 1 0
                   0 0 1)
           (get-child-transformation/search
            p (list pp)))]
      [else
       (get-child-transformation/search
        p pp)]))))

;; Transformations here are represented by 3x3
;; matricies to allow for easy composition. This
;; converts back to the 2x3 form used by dc<%>
(define (convert-affine-form v)
  (match-define (vector a b e c d f _ _ _) v)
  (vector
   a b
   c d
   e f))

(define (get-child-transformation/search pict list)
  (cond  [(empty? list)
          (vector 1 0 0
                  0 1 0
                  0 0 1)]
         [else
          (define t
            (find-next-target pict (first list)))
          (transformation-compose
           (get-child-transformation/search (first list) (rest list))
           t)]))


(define (find-next-target p target)
  (let loop ([p p]
             [children (pict-children p)]
             [foundk (lambda (t) t)]
             [failk (lambda () (error 'explain-child "could not find child pict"))])       
    (match children
      [(list) (failk)]
      [(cons r h)
       #:when (pict-path-element=? (child-pict r) target)
       (foundk (child->transformation p r))]
      [(cons r h)
       (loop
        (child-pict r)
        (pict-children (child-pict r))
        (lambda (t) (foundk (transformation-compose (child->transformation p r) t)))
        (lambda () (loop p h foundk failk)))])))
      
(define (transformation-compose1 t1 t2)
  (define w 3)
  (for*/vector #:length 9 #:fill 0
    ([i (in-range w)]
     [j (in-range w)])
    (for/sum ([k (in-range w)])
      (* (vector-ref t1 (+ (* i w) k))
         (vector-ref t2 (+ (* w k) j))))))

(define (transformation-compose t1 . t2)
  (match t2
    [(list) t1]
    [(cons r h)
     (apply
      transformation-compose
      (transformation-compose1 t1 r)
      h)]))

(define (transformation-apply t v)
  (define w 3)
  (for/vector #:length 3 #:fill 0
    ([i (in-range w)])
    (for/sum ([j (in-range w)])
      (* (vector-ref v j) (vector-ref t (+ (* i w) j))))))

;; switch from the inverted coordinates used by
;; pict-child to normal drawing coordinates.
(define (uninvert-coordinartes p c t)
  (match-define (vector sx sxy dx syx sy dy _ _ _) t)
  (match-define
    (vector _ _ ndx _ _ ndy _ _ _)
    (transformation-compose
     (vector
      1 0 0
      0 -1 (pict-height p)
      0 0 1)
     t
     (vector
      1 0 0
      0 -1 (pict-height c)
      0 0 1)))
  (vector
   sx sxy ndx
   syx sy ndy
   0 0 1))

(define (child->transformation parent c)
  (vector
   (child-sx c) (child-sxy c) (child-dx c)
   (child-syx c) (child-sy c) (child-dy c)
   0 0 1))