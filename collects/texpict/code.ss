(module code mzscheme
  (require "mrpict.ss"
	   (lib "class.ss")
           (lib "mred.ss" "mred")
	   (lib "unitsig.ss"))

  (provide code code^ code-params^ code@)

  (define-syntax (code stx)
    (with-syntax ([typeset-code (datum->syntax-object stx 'typeset-code)])
      (syntax-case stx ()
	[(_ expr) (syntax/loc stx
		    (typeset-code (quasisyntax ((... ...) expr))))]
	[(_ expr ...) (syntax/loc stx
			(typeset-code (quasisyntax (code:line ((... ...) expr) ...))))])))
      
  (define-signature code^
    (typeset-code 
     comment-color keyword-color id-color literal-color
     code-align
     current-keyword-list  current-const-list))

  (define-signature code-params^
    (current-font-size 
     line-sep))

  (define code@
    (unit/sig code^
      (import code-params^)
      
      (define (tt s)
	(text s `(bold . modern) (current-font-size)))

      (define (code-align p)
	(lift (inset p 0 (pict-height p) 0 0) (pict-height p)))
      
      (define current-keyword-list 
	(make-parameter '("define" "cond" "define-struct" "and" "or" "else")))
      (define current-const-list 
	(make-parameter '("null")))
      
      (define base-color "brown")
      (define keyword-color "black")
      (define id-color "navy")
      (define literal-color (make-object color% 51 135 39))
      (define comment-color base-color)
      
      (define open-paren-p (colorize (tt "(") base-color))
      (define close-paren-p (colorize (tt ")") base-color))
      (define open-sq-p (colorize (tt "[") base-color))
      (define close-sq-p (colorize (tt "]") base-color))
      (define quote-p (colorize (tt "'") literal-color))
      (define semi-p (colorize (tt "; ") comment-color))
      (define open-paren/lit-p (colorize (tt "(") literal-color))
      (define close-paren/lit-p (colorize (tt ")") literal-color))
      (define open-paren/tmpl-p (colorize (tt "(") comment-color))
      (define close-paren/tmpl-p (colorize (tt ")") comment-color))
      
      (define (add-close p closes)
	(cond
	 [(null? closes) p]
	 [(memq (car closes) '(comment line))
	  (add-close p (cdr closes))]
	 [else
	  (add-close (htl-append p (case (car closes)
				     [(literal) close-paren/lit-p]
				     [(template) close-paren/tmpl-p]
				     [(cond template-cond) close-sq-p]
				     [else close-paren-p]))
		     (cdr closes))]))

      (define (pad-left space p)
	(if (= 0 space)
	    p
	    (htl-append (tt (make-string space #\space)) p)))

      (define (pad-bottom space p)
	(if (= 0 space)
	    p
	    (vl-append (tt " ") (pad-bottom (sub1 space) p))))

      (define (colorize-id str mode)
	(if (char=? #\_ (string-ref str 0))
	    (colorize (text (substring str 1) `(italic . modern) (current-font-size))
		      id-color)
	    (colorize
	     (tt str)
	     (cond
	      [(eq? mode 'literal) literal-color]
	      [(memq mode '(comment template)) comment-color]
	      [(member str (current-keyword-list)) keyword-color]
	      [(member str (current-const-list)) literal-color]
	      [else id-color]))))

      (define (sub-mode mode)
	(case mode
	  [(line cond) #f]
	  [(template-cond) 'template]
	  [else mode]))

      (define (cond? s)
	(eq? 'cond (syntax-e s)))

      (define (get-span stx)
	(syntax-case stx (code:blank)
	  [code:blank 1]
	  [_ (or (syntax-span stx) 1)]))
      
      (define (typeset-code stx)
	(let loop ([stx stx][closes null][mode #f])
	  (syntax-case stx (quote syntax-unquote 
				  code:contract code:comment code:line
				  code:template code:blank $)
	    [() (add-close (htl-append open-paren-p close-paren-p) closes)]
	    [code:blank (tt " ")]
	    [$ (colorize-id "|" closes)]
	    [(quote x)
	     (htl-append quote-p (loop #'x closes 'literal))]
	    [(code:contract i ...)
	     (htl-append semi-p (loop (datum->syntax-object #f (syntax->list #'(i ...)))
				      closes 'comment))]
	    [(code:line i ...)
	     (loop (datum->syntax-object #f (syntax->list #'(i ...)))
		   closes 'line)]
	    [(code:comment s)
	     (htl-append semi-p (colorize (tt (syntax-e #'s)) comment-color))]
	    [(code:template i)
	     (let loop ([p (loop #'i closes 'template)] [semis semi-p])
	       (if ((pict-height p) . > . (pict-height semis))
		   (loop p (vl-append line-sep semi-p semis))
		   (htl-append semis p)))]
	    [(i ...)
	     (let ([is (syntax->list #'(i ...))])
	       ;; Convert each i to a picture, include close paren in last item:
	       (let ([ips (let iloop ([is is][sub-mode (sub-mode mode)])
			    (cond
			     [(null? (cdr is)) (list (loop (car is) (cons mode closes) sub-mode))]
			     [else (cons (loop (car is) null sub-mode)
					 (iloop (cdr is) (if (cond? (car is))
							     (if (eq? mode 'template)
								 'template-cond
								 'cond)
							     sub-mode)))]))])
		 ;; Combine the parts:
		 (let ([left (or (syntax-column stx) +inf.0)])
		   (let loop ([stxs is]
			      [ps ips]
			      [line-so-far (case mode
					     [(literal) open-paren/lit-p]
					     [(template) open-paren/tmpl-p]
					     [(comment line) (blank)]
					     [(cond template-cond) open-sq-p]
					     [else open-paren-p])]
			      [col (+ left 1)]
			      [line (syntax-line stx)]
			      [always-space? #f])
		     (cond
		      [(null? ps) (blank)]
		      [(or (not line)
			   (= line (or (syntax-line (car stxs)) line)))
		       (let* ([space (if (syntax-column (car stxs))
					 (inexact->exact
					  (max (if always-space? 1 0) (- (syntax-column (car stxs)) col)))
					 (if always-space? 1 0))]
			      [p (htl-append
				  line-so-far
				  (pad-left space (car ps)))])
			 (if (null? (cdr stxs))
			     p
			     (loop (cdr stxs)
				   (cdr ps)
				   p
				   (if (not (syntax-column (car stxs)))
				       +inf.0
				       (+ col space (get-span (car stxs))))
				   (or line (syntax-line (car stxs)))
				   #t)))]
		      [else
		       (vl-append
			line-so-far
			(let* ([space (max 0 (- (or (syntax-column (car stxs)) 0) left))]
			       [p (pad-left space (car ps))])
			  (if (null? (cdr stxs))
			      p
			      (loop (cdr stxs)
				    (cdr ps)
				    p
				    (+ left space (get-span (car stxs)))
				    (or (syntax-line (car stxs)) (add1 line))
				    #t))))])))))]
	    [id
	     (identifier? stx)
	     (add-close (colorize-id (symbol->string (syntax-e stx)) mode) closes)]
	    [else
	     (add-close (if (pict? (syntax-e stx))
			    (syntax-e stx)
			    (colorize (tt (format "~s" (syntax-e stx))) literal-color))
			closes)])))
      
      )))
