
(module utils mzscheme
  (require (lib "class.ss"))

  (require (lib "mred.ss" "mred"))

  (require "mrpict.ss")

  ;; Utilities for use with mrpict
  
  (provide cons-colorized-picture
	   color-frame
	   round-frame
	   color-round-frame
	   color-dash-frame

	   arrow
	   arrowhead
	   arrow-line
	   arrows-line
	   
	   circle
	   disk

	   cloud
	   file-icon

	   add-line
	   add-arrow-line
	   add-arrows-line

	   bitmap

	   scale)

  (define (re-pict box naya)
    (let ([w (pict-width box)]
	  [h (pict-height box)]
	  [d (pict-descent box)]
	  [a (pict-ascent box)])
      (make-pict (pict-draw naya)
		 w h
		 a d
		 (list (make-child box 0 0)))))
  
  (define cons-colorized-picture
    (lambda (p color cmds)
      (re-pict
       p
       (cc-superimpose
	p
	(colorize
	 (cons-picture
	  (ghost (launder p))
	  cmds)
	 color)))))

  (define (round-frame p radius)
    (re-pict
     p
     (cc-superimpose
      p
      (let ([w (pict-width p)]
	    [h (pict-height p)])
	(dc (lambda (dc x y)
	      (let ([b (send dc get-brush)])
		(send dc set-brush (send the-brush-list find-or-create-brush
					 "white" 'transparent))
		(send dc draw-rounded-rectangle x y w h radius)
		(send dc set-brush b)))
	    (pict-width p) (pict-height p) 0 0)))))

  ;; FIXME: abstract common part of color-frame, etc.

  (define color-frame
    (case-lambda
     [(p color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (frame (ghost (launder p))) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p color) (color-frame p color #f)]))
  
  (define color-round-frame
    (case-lambda
     [(p radius color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (round-frame (ghost (launder p)) radius) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p radius color) (color-round-frame p radius color #f)]))  

  (define color-dash-frame
    (case-lambda
     [(p seg-length color w)
      (re-pict
       p
       (cc-superimpose
	p
	(let ([p2 (colorize (dash-frame (ghost (launder p)) seg-length) color)])
	  (if w
	      (linewidth w p2)
	      p2))))]
     [(p seg-length color) (color-dash-frame p seg-length color #f)]))  

  ;; Returns three values: pict dx dy
  (define (generic-arrow stem? solid? size angle)
    (values
     (dc
      (lambda (dc x y)
	(define (pt->xform-obj p)
	  (let* ([x (car p)]
		 [y (cadr p)]
		 [d (sqrt (+ (* x x) (* y y)))]
		 [a (atan y x)])
	    (make-object point% 
			 (* d size 1/2 (cos (+ a angle)))
			 (* d size 1/2 (- (sin (+ a angle)))))))
	(let ([b (send dc get-brush)]
	      [p (send dc get-pen)])
	  (send dc set-pen (send the-pen-list
				 find-or-create-pen
				 (send p get-color)
				 0
				 'solid))
	  (send dc set-brush (send the-brush-list
				   find-or-create-brush
				   (send p get-color)
				   (if solid? 'solid 'transparent)))
	  (send dc draw-polygon 
		(map pt->xform-obj
		     (if stem?
			 `((1 0)
			   (0 -1)
			   (0 -1/2)
			   (-1 -1/2)
			   (-1 1/2)
			   (0 1/2)
			   (0 1))
			 `((1 0)
			   (-1 -1)
			   (-1/2 0)
			   (-1 1))))
		(+ x (/ size 2)) (+ y (/ size 2)))
	  (send dc set-brush b)
	  (send dc set-pen p)))
      size size 0 0)
     (- (- 0 (* 1/2 size (cos angle))) (/ size 2))
     (- (+ (* 1/2 size) (- (* 1/2 size (sin angle)))) size)))

  (define (arrow/delta size angle)
    (generic-arrow #t #t size angle))
  (define (arrow size angle)
    (let-values ([(p dx dy) (arrow/delta size angle)])
      p))

  (define (arrowhead/delta size angle)
    (generic-arrow #f #t size angle))
  (define (arrowhead size angle)
    (let-values ([(p dx dy) (arrowhead/delta size angle)])
      p))

  (define (arrow-line dx dy size)
    (let-values ([(a adx ady) (arrowhead/delta size (atan dy dx))])
      (picture
       0 0
       `((connect 0 0 ,dx ,dy)
	 (place ,(+ dx adx) ,(+ ady dy) ,a)))))

  (define (arrows-line dx dy size)
    (picture
     0 0
     `((place 0 0 ,(arrow-line dx dy size))
       (place ,dx ,dy ,(arrow-line (- dx) (- dy) size)))))

  (define (circle size)
    (dc (lambda (dc x y)
	  (let ([b (send dc get-brush)])
	    (send dc set-brush (send the-brush-list find-or-create-brush
				     "white" 'transparent))
	    (send dc draw-ellipse x y size size)
	    (send dc set-brush b)))
	size size 0 0))

  (define (disk size)
    (dc (lambda (dc x y)
	  (send dc draw-ellipse x y size size))
	size size 0 0))

  (define cloud
    (case-lambda
     [(w h) (cloud w h "gray")]
     [(w h color)
      (dc
       (lambda (dc x y)
	 (let ([b (send dc get-brush)]
	       [p (send dc get-pen)])
	   (send dc set-pen (send the-pen-list
				  find-or-create-pen
				  "white" 0 'transparent))
	   (send dc set-brush (send the-brush-list
				    find-or-create-brush
				    color
				    'solid))
	   (send dc draw-ellipse
		 x (+ y (* 1/4 h))
		 (* 1/2 w) (* 1/2 h))
	   (send dc draw-ellipse
		 (+ x (* 1/5 w)) y
		 (* 3/5 w) (* 2/5 h))
	   (send dc draw-ellipse
		 (+ x (* 1/5 w)) (+ y (* 1/3 h))
		 (* 3/5 w) (* 2/3 h))
	   (send dc draw-ellipse
		 (+ x (* 3/5 w)) (+ y (* 1/4 h))
		 (* 2/5 w) (* 1/3 h))
	   (send dc draw-ellipse
		 (+ x (* 3/5 w)) (+ y (* 1/2 h))
		 (* 2/5 w) (* 1/3 h))

	   (send dc set-brush b)
	   (send dc set-pen p)))
       w h 0 0)]))

  (define (file-icon w h gray)
    (dc
     (let* ([sw (lambda (x) (* (/ w 110) x))]
	    [sh (lambda (y) (* (/ h 150) y))]
	    [->pt (lambda (l)
		    (map (lambda (p)
			   (make-object point% 
					(sw (car p))
					(sh (cadr p))))
			 l))])
       (lambda (dc x y)
	 (define p (send dc get-pen))
	 (define b (send dc get-brush))

	 (let ([color (send the-brush-list
			    find-or-create-brush
			    (cond
			     [(or (string? gray) (is-a? gray color%)) gray]
			     [gray (make-object color% 200 200 255)]
			     [else "white"])
			    'solid)])

	   (send dc set-pen (send the-pen-list 
				  find-or-create-pen "black" 
				  (send p get-width)
				  'solid))
	   (send dc set-brush color)
	   
	   (send dc draw-polygon 
		 (->pt '((0 0)
			 (0 150)
			 (110 150)
			 (110 20)
			 (90 0)))
		 x y)

	   (send dc draw-line (+ x (sw 90)) y (+ x (sw 90)) (+ y (sh 20)))
	   (send dc draw-line (+ x (sw 90)) (+ y (sh 20)) (+ x (sw 110)) (+ y (sh 20))))
	 
	 (send dc set-brush b)
	 (send dc set-pen p)))
     w h 0 0))

  (define (-add-line base src find-src dest find-dest thickness color arrow-size arrow2-size)
    (let-values ([(sx sy) (find-src base src)]
		 [(dx dy) (find-dest base dest)])
      (cc-superimpose
       base
       (let ([p (cons-picture
		 (ghost (launder base))
		 `((connect ,sx ,sy ,dx ,dy)
		   ,@(if arrow-size
			 (let-values ([(arrow xo yo)
				       (arrowhead/delta 
					arrow-size 
					(atan (- dy sy) 
					      (- dx sx)))])
			   `((place ,(+ dx xo) ,(+ dy yo) ,arrow)))
			 null)
		   ,@(if arrow2-size
			 (let-values ([(arrow xo yo)
				       (arrowhead/delta 
					arrow-size 
					(atan (- sy dy) 
					      (- sx dx)))])
			   `((place ,(+ sx xo) ,(+ sy yo) ,arrow)))
			 null)))])
	 (let ([p2 (if thickness
		       (linewidth thickness p)
		       p)])
	   (if color
	       (colorize p2 color)
	       p2))))))

  (define add-line
    (case-lambda
     [(base src find-src dest find-dest)
      (add-line base src find-src dest find-dest #f #f)]
     [(base src find-src dest find-dest thickness)
      (add-line base src find-src dest find-dest thickness #f)]
     [(base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color #f #f)]))

  (define add-arrow-line
    (case-lambda
     [(arrow-size base src find-src dest find-dest)
      (add-arrow-line arrow-size base src find-src dest find-dest #f #f)]
     [(arrow-size base src find-src dest find-dest thickness)
      (add-arrow-line arrow-size base src find-src dest find-dest thickness #f)]
     [(arrow-size base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color arrow-size #f)]))

  (define add-arrows-line
    (case-lambda
     [(arrow-size base src find-src dest find-dest)
      (add-arrows-line arrow-size base src find-src dest find-dest #f #f)]
     [(arrow-size base src find-src dest find-dest thickness)
      (add-arrows-line arrow-size base src find-src dest find-dest thickness #f)]
     [(arrow-size base src find-src dest find-dest thickness color)
      (-add-line base src find-src dest find-dest thickness color arrow-size arrow-size)]))
  
  (define (bitmap filename)
    (let ([bm (make-object bitmap% filename)])
      (let ([w (send bm get-width)]
	    [h (send bm get-height)])
	(dc
	 (lambda (dc x y)
	   (send dc draw-bitmap bm x y))
	 w h 0 0))))

  (define scale
    (case-lambda
     [(p x-factor y-factor)
      (let ([drawer (make-pict-drawer p)])
	(dc
	 (lambda (dc x y)
	   (define (reset-pen)
	     (let ([p (send dc get-pen)])
	       (send dc set-pen (send the-pen-list
				      find-or-create-pen
				      "white" 1 'transparent))
	       (send dc set-pen p)))
	   (let-values ([(xs ys) (send dc get-scale)])
	     (send dc set-scale (* xs x-factor) (* ys y-factor))
	     (reset-pen)
	     (drawer dc
		     (/ x x-factor)
		     (/ y y-factor))
	     (send dc set-scale xs ys)
	     (reset-pen)))
	 (* (pict-width p) x-factor)
	 (* (pict-height p) y-factor)
	 (* (pict-ascent p) y-factor)
	 (* (pict-descent p) y-factor)))]
     [(p factor) (scale p factor factor)])))
