#lang scheme/base

(require (rename-in "pict.rkt"
                    [hline t:hline]
                    [vline t:vline]
                    [frame t:frame])
         "convertible.rkt"
         (rename-in "utils.rkt"
                    [pin-line t:pin-line]
                    [pin-arrow-line t:pin-arrow-line]
                    [pin-arrows-line t:pin-arrows-line])
         (only-in racket/draw dc-path% make-bitmap bitmap% bitmap-dc% color%)
         (only-in racket/class new send make-object is-a?/c)
         racket/contract)

(define (hline w h #:segment [seg #f])
  (if seg
      (dash-hline w h seg)
      (t:hline w h)))

(define (vline w h #:segment [seg #f])
  (if seg
      (dash-vline w h seg)
      (t:vline w h)))

(define (frame p
               #:color [col #f]
               #:line-width [lw #f]
               #:segment [seg #f])
  (let* ([p (pict-convert p)]
         [f (if seg
                (dash-frame (launder (ghost p)) seg)
                (t:frame (launder (ghost p))))]
         [f (if col
                (colorize f col)
                f)]
         [f (if lw 
                (linewidth lw f)
                f)])
    (refocus (cc-superimpose p f)
             p)))

(define (pict-path? p)
  (or (pict-convertible? p)
      (and (pair? p)
           (list? p)
           (andmap pict-convertible? p))))

(define (label-line label pict src-pict src-coord-fn dest-pict dest-coord-fn
                    #:x-adjust [x-adjust 0] #:y-adjust [y-adjust 0])
  (let-values ([(src-x src-y) (src-coord-fn pict src-pict)]
               [(dest-x dest-y) (dest-coord-fn pict dest-pict)])
    (let* ([src (make-rectangular src-x src-y)]
           [dest (make-rectangular dest-x dest-y)]
           [adjust (make-rectangular x-adjust y-adjust)]
           [v (- dest src)]
           [h2 (pict-height label)])
      ;; Ensure that the src is left of dest
      (when (< (real-part v) 0)
        (set! v (- v))
        (set! src dest))
      (let ([p (+ src
                  ;; Move the label to sit atop the line.
                  (/ (* h2 -i v) (magnitude v) 2)
                  ;; Center the label in the line.
                  (/ (- v (make-rectangular (pict-width label)
                                            (pict-height label)))
                     2)
                  adjust)])
        (pin-over
         pict
         (real-part p)
         (imag-part p)
         label)))))

(define (pin-line p 
                  src src-find
                  dest dest-find
                  #:start-angle [sa #f] #:end-angle [ea #f]
                  #:start-pull [sp #f] #:end-pull [ep #f]
                  #:color [col #f]
                  #:alpha [alpha 1.0]
                  #:line-width [lw #f]
                  #:under? [under? #f]
                  #:solid? [solid? #t]
                  #:style [style #f]
                  #:label [label #f]
                  #:x-adjust-label [x-adjust 0]
                  #:y-adjust-label [y-adjust 0])
  (define line
    (if (not (or sa ea))
        (finish-pin (launder (t:pin-line (ghost p)
                                         src src-find 
                                         dest dest-find
                                         #:style style))
                    p lw col alpha under?)
        (pin-curve* #f #f p src src-find dest dest-find
                    sa ea sp ep 0 col lw under? #t
                    style alpha)))
  (if label
      (label-line label line src src-find dest dest-find
                  #:x-adjust x-adjust #:y-adjust y-adjust)
      line))

(define (pin-arrow-line sz p 
                        src src-find
                        dest dest-find
                        #:start-angle [sa #f] #:end-angle [ea #f]
                        #:start-pull [sp #f] #:end-pull [ep #f]
                        #:color [col #f]
                        #:alpha [alpha 1.0]
                        #:line-width [lw #f]
                        #:under? [under? #f]
                        #:solid? [solid? #t]
                        #:style [style #f]
                        #:hide-arrowhead? [hide-arrowhead? #f]
                        #:label [label #f]
                        #:x-adjust-label [x-adjust 0]
                        #:y-adjust-label [y-adjust 0])
  (define line
    (if (not (or sa ea))
        (finish-pin (launder (t:pin-arrow-line sz (ghost p)
                                               src src-find 
                                               dest dest-find
                                               #f #f #f solid?
                                               #:hide-arrowhead? hide-arrowhead?
                                               #:style style))
                    p lw col alpha under?)
        (pin-curve* #f (not hide-arrowhead?) p src src-find dest dest-find
                    sa ea sp ep sz col lw under? solid?
                    style alpha)))
  (if label
      (label-line label line src src-find dest dest-find
                  #:x-adjust x-adjust #:y-adjust y-adjust)
      line))
  
(define (pin-arrows-line sz p 
                         src src-find
                         dest dest-find
                         #:start-angle [sa #f] #:end-angle [ea #f]
                         #:start-pull [sp #f] #:end-pull [ep #f]
                         #:color [col #f]
                         #:alpha [alpha 1.0]
                         #:line-width [lw #f]
                         #:under? [under? #f]
                         #:solid? [solid? #t]
                         #:style [style #f]
                         #:hide-arrowhead? [hide-arrowhead? #f]
                         #:label [label #f]
                         #:x-adjust-label [x-adjust 0]
                         #:y-adjust-label [y-adjust 0])
  (define line
    (if (not (or sa ea))
        (finish-pin (launder (t:pin-arrows-line sz (ghost p)
                                                src src-find 
                                                dest dest-find
                                                #f #f #f solid?
                                                #:hide-arrowhead? hide-arrowhead?
                                                #:style style))
                    p lw col alpha under?)
        (pin-curve* (not hide-arrowhead?) (not hide-arrowhead?)
                    p src src-find dest dest-find
                    sa ea sp ep sz col lw under? solid? 
                    style alpha)))
  (if label
      (label-line label line src src-find dest dest-find
                  #:x-adjust x-adjust #:y-adjust y-adjust)
      line))
    
(define (pin-curve* start-arrow? end-arrow? p 
                    src src-find
                    dest dest-find
                    sa ea sp ep
                    sz col lw
                    under? solid?
                    style alpha)
  (let-values ([(sx0 sy0) (src-find p src)]
               [(dx0 dy0) (dest-find p dest)])
    (let* ([sa (or sa
                   (atan (- sy0 dy0) (- dx0 sx0)))]
           [ea (or ea
                   (atan (- sy0 dy0) (- dx0 sx0)))]
           [d (sqrt (+ (* (- dy0 sy0) (- dy0 sy0)) (* (- dx0 sx0) (- dx0 sx0))))]
           [sp (* (or sp 1/4) d)]
           [ep (* (or ep 1/4) d)])
      (let ([dx (if end-arrow? (- dx0 (* sz (cos ea))) dx0)]
            [dy (if end-arrow? (+ dy0 (* sz (sin ea))) dy0)]
            [sx (if start-arrow? (+ sx0 (* sz (cos sa))) sx0)]
            [sy (if start-arrow? (- sy0 (* sz (sin sa))) sy0)]
            [path (new dc-path%)]
            [maybe-pin-line
             (lambda (arrow? p sx sy dx dy)
               (if arrow?
                   (pin-arrow-line 
                    sz
                    p
                    p (lambda (a b) (values sx sy))
                    p (lambda (a b) (values dx dy))
                    #:line-width lw
                    #:color col
                    #:under? under?
                    #:solid? solid?
                    #:style style)
                   p))])
        (send path move-to sx sy)
        (send path curve-to
              (+ sx (* sp (cos sa)))
              (- sy (* sp (sin sa)))
              (- dx (* ep (cos ea)))
              (+ dy (* ep (sin ea)))
              dx
              dy)
        (maybe-pin-line 
         start-arrow?
         (maybe-pin-line 
          end-arrow?
          ((if under? pin-under pin-over)
           p
           0 0
           (let* ([p (dc (lambda (dc x y)
                           (let ([b (send dc get-brush)])
                             (send dc set-brush "white" 'transparent)
                             (send dc draw-path path x y)
                             (send dc set-brush b)))
                         0 0)]
                  [p (if col
                         (colorize p col)
                         p)]
                  [p (if (= alpha 1.0)
                         p
                         (cellophane p alpha))]
                  [p (if lw
                         (linewidth lw p)
                         p)]
                  [p (if style
                         (linestyle style p)
                         p)])
             p))
          dx dy dx0 dy0)
         sx sy sx0 sy0)))))


(define (finish-pin l p lw col alpha under?)
  (let* ([l (if lw
                (linewidth lw l)
                l)]
         [l (if col
                (colorize l col)
                l)]
         [l (if (= alpha 1.0)
                l
                (cellophane l alpha))])
    (if under?
        (cc-superimpose l p)
        (cc-superimpose p l))))

(define fish
  (let ([standard-fish
         (lambda (w h 
                    #:direction [direction 'left]
                    #:color [color "blue"]
                    #:eye-color [eye-color "black"]
                    #:open-mouth [open-mouth #f])
           (standard-fish w h direction color eye-color open-mouth))])
    standard-fish))

(define (pict->bitmap p [smoothing 'aligned]
                      #:make-bitmap [make-bitmap make-bitmap])
  (define w (pict-width p))
  (define h (pict-height p))
  (define bm (make-bitmap (max 1 (inexact->exact (ceiling w)))
                          (max 1 (inexact->exact (ceiling h)))))
  (unless (send bm ok?)
    (error 'pict->bitmap
           (string-append "bitmap creation failed\n"
                          "  possible reason: out of memory\n"
                          "  pict width: ~a\n"
                          "  pict height: ~a")
           w
           h))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-smoothing smoothing)
  (draw-pict p dc 0 0)
  bm)
  
(define (pict->argb-pixels p [smoothing 'aligned])
  (define bm (pict->bitmap p smoothing))
  (define w (send bm get-width)) 
  (define h (send bm get-height))
  (define bytes (make-bytes (* w h 4)))
  (send bm get-argb-pixels 0 0 w h bytes)
  bytes)
  
(define (argb-pixels->pict b w)
  (define h (/ (bytes-length b) w 4))
  (define bm (make-bitmap w (/ (bytes-length b) w 4)))
  (send bm set-argb-pixels 0 0 w h b)
  (bitmap bm))

(define (freeze p)
  (define p* (pict-convert p))
  (define frozen (bitmap (pict->bitmap p*)))
  (struct-copy pict p* [draw (pict-draw frozen)]))

(provide hline vline
         frame
         pict-path?
         pin-line pin-arrow-line pin-arrows-line
         freeze


         dc-for-text-size
         convert-bounds-padding
         show-pict
         current-expected-text-scale
         dc
         linewidth
         linestyle

         draw-pict
         make-pict-drawer

         (contract-out
          [text (->* (string?)
                     (text-style/c 
                      (and/c (between/c 1 1024) integer?)
                      number?)
                     pict?)])

         text-style/c

         (struct-out pict)
         (struct-out child)

         black-and-white

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


         launder  ; pict -> pict

         blank        ; -> pict
         ;; w h -> pict
         ;; w h d -> pict

         clip-descent   ; pict -> pict
         clip-ascent    ; pict -> pict
         baseless       ; pict -> pict
         inset          ; pict i -> pict
         ; pict hi vi -> pict
         ; pict l t r b -> pict
         refocus        ; pict pict -> pict
         panorama       ; pict -> pict

         use-last       ; pict pict -> pict
         use-last*      ; pict pict -> pict

         hline        ; w h -> pict
         vline        ; w h -> pict
         
         frame        ; pict -> pict
         
         
         
         ghost        ; pict -> pict

         
         vl-append    ; d pict ... -> pict ; d units between each picture
         vc-append
         vr-append
         ht-append
         hc-append
         hb-append
         htl-append       ; align bottoms of ascents
         hbl-append       ; align tops of descents (normal text alignment)

         lt-superimpose ; pict ... -> pict
         lb-superimpose
         lc-superimpose
         ltl-superimpose
         lbl-superimpose
         rt-superimpose
         rb-superimpose
         rc-superimpose
         rtl-superimpose
         rbl-superimpose
         ct-superimpose
         cb-superimpose
         cc-superimpose
         ctl-superimpose
         cbl-superimpose

         table ; ncols pict-list col-aligns row-aligns col-seps row-seps -> pict

         colorize ; pict color-string -> pict

         pin-over
         pin-under
         drop-below-ascent
         lift-above-baseline

         (except-out (all-from-out "utils.rkt")
                   
                     color-frame color-dash-frame
                     round-frame color-round-frame
    
                     cons-colorized-picture
                     arrow-line
                     arrows-line

                     add-line
                     add-arrow-line
                     add-arrows-line

                     explode-star

                     cloud
                     file-icon
                     standard-fish
                     jack-o-lantern
                     angel-wing
                     desktop-machine
                     thermometer

                     find-pen find-brush)
         (contract-out
          [cloud (->* [real? real?]
                      [(or/c string? (is-a?/c color%))
                       #:style (listof (or/c 'square 'nw 'ne 'sw 'se 'wide))]
                      pict?)]
          [file-icon (->* [real? real? any/c] [any/c] pict?)]
          [rename fish standard-fish (->* [real? real?]
                                          [#:direction (or/c 'left 'right)
                                           #:color (or/c string? (is-a?/c color%))
                                           #:eye-color (or/c string? (is-a?/c color%) #f)
                                           #:open-mouth (or/c boolean? (between/c 0 1))]
                                          pict?)]
          [jack-o-lantern (->* [real?]
                               [(or/c string? (is-a?/c color%))
                                (or/c string? (is-a?/c color%))
                                (or/c string? (is-a?/c color%))]
                               pict?)]
          [angel-wing (-> real? real? any/c pict?)]
          [desktop-machine (->* [real?] [(listof symbol?)] pict?)]
          [thermometer (->* []
                            [#:height-% (between/c 0 1)
                             #:color-% (between/c 0 1)
                             #:ticks exact-nonnegative-integer?
                             #:start-color (or/c string? (is-a?/c color%))
                             #:end-color (or/c string? (is-a?/c color%))
                             #:top-circle-diameter (>/c 0)
                             #:bottom-circle-diameter (>/c 0)
                             #:stem-height (>/c 0)
                             #:mercury-inset (>/c 0)]
                            pict?)])
         pict->bitmap
         pict->argb-pixels
         argb-pixels->pict)
