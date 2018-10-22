#lang scribble/doc
@(require "pict-diagram.rkt"
          scribble/eval scribble/manual
          pict/face pict
          (for-label racket/gui racket/draw
                     slideshow/base slideshow/code
                     pict/flash pict/face pict/balloon
                     (except-in racket only drop)
                     pict
                     pict/convert))


@(define ss-eval (make-base-eval))
@(ss-eval '(require pict racket/math racket/class racket/draw
                    racket/list pict/balloon pict/flash))

@title[#:style 'toc]{Pict: Functional Pictures}

@declare-exporting[pict]

@defmodule*/no-declare[(pict)]{ The
@racketmodname[pict] library is one of the standard Racket
functional picture libraries (the other being @racketmodname[2htdp/image #:indirect]).
This library was originally designed for use with
@seclink[#:doc '(lib "scribblings/slideshow/slideshow.scrbl") "top"]{Slideshow},
and is re-provided by the @racketmodname[slideshow] language.}

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section{Pict Datatype}

A @deftech{pict} is a @racket[pict] structure representing an image.
Some functions, such as
@racket[hline], create new simple picts. Other functions, such as
@racket[ht-append], build new picts out of existing picts. In the
latter case, the embedded picts retain their identity, so that
offset-finding functions, such as @racket[lt-find], can find the
offset of an embedded pict in a larger pict.

In addition to its drawing part, a pict has the following
@deftech{bounding box} structure:

@centerline[pict-diagram]

That is, the bounding box has a width @math{w} and a height
@math{h}. For a single text line, @math{d} is descent below the
baseline, and @math{a+d=h}. For multiple text lines (often created
with a function like @racket[vc-append]), @math{a} is the ascent of
the top line, and @math{d} is the descent of the bottom line, so
@math{a+d<h}. Many picts have @math{d=0} and @math{a=h}.

In addition, a pict can have a @defterm{last} sub-pict that
corresponds to the last item on the last line of text, so that extra
lines can be added to the last line. In particular, the @defterm{last}
element is useful for adding closing parentheses to a block of Racket
code, where the last line of code not the longest line in the block.

The size information for a pict is computed when the pict is
created. This strategy supports programs that create new picts though
arbitrarily complex computations on the size and shape of existing
picts. The functions @racket[pict-width], @racket[pict-height],
@racket[pict-descent], and @racket[pict-ascent] extract bounding box
information from a pict.

A pict is a convertible datatype through the
@racketmodname[file/convertible] protocol. Supported conversions
include @racket['png-bytes], @racket['eps-bytes], @racket['pdf-bytes],
@racket['svg-bytes], and variants such as @racket['png-bytes+bounds]
and @racket['png-bytes+bounds8].

A pict is serializable via @racketmodname[racket/serialize], but
serialization loses sub-pict information (preserving only the pict's
drawing and bounding box).

@history[#:changed "1.2" @elem{Added support for
                               @racket['png-bytes+bounds],
                               @racket['png-bytes+bounds8] and similar
                               variants.}
         #:changed "1.3" @elem{Enabled serialization.}]


@defstruct[pict ([draw any/c]
                 [width real?]
                 [height real?]
                 [ascent real?]
                 [descent real?]
                 [children (listof child?)]
                 [panbox (or/c #f any/c)]
                 [last (or/c #f pict-path?)])]{

A @racket[pict] structure is normally not created directly with
@racket[make-pict]. Instead, functions like @racket[text],
@racket[hline], and @racket[dc] are used to construct a pict.

The @racket[draw] field contains the pict's drawing information in an
internal format. Roughly, the drawing information is a procedure that
takes a @racket[dc<%>] drawing context and an offset for the pict's
top-left corner (i.e., it's @tech{bounding box}'s top left corner relative to
the @racket[dc<%>] origin). The state of the @racket[dc<%>] is
intended to affect the pict's drawing; for example, the pen and brush
will be set for a suitable default drawing mode, and the
@racket[dc<%>] scale will be set to scale the resulting image. Use
@racket[draw-pict] (as opposed to @racket[pict-draw]) to draw the
picture.

The @racket[panbox] field is internal and initialized to @racket[#f].

The @racket[last] field indicates a pict within the @racket[children]
list (transitively) that can be treated as the last element of the
last line in the pict. A @racket[#f] value means that the pict is its
own last sub-pict.}


@defstruct[child ([pict pict?]
                  [dx real?]
                  [dy real?]
                  [sx real?]
                  [sy real?]
                  [sxy real?]
                  [syx real?])]{

Records, for a pict constructed of other picts, the transformation to
arrive at a @tech{inverted point} in the composed pict from an
@tech{inverted point} in a constituent pict's. An @deftech{inverted
point} is a point relative to a pict's lower-left corner with an
increasing value moving upward.

A @racket[child] structure is normally not created directly with
@racket[make-child]. Instead, functions like @racket[hc-append] create
@racket[child] structures when combining picts to create a new one.}

@; ------------------------------------------------------------------------

@section{Basic Pict Constructors}

@defproc[(dc [draw (-> (is-a?/c dc<%>) real? real? any)]
             [w real?]
             [h real?]
             [a real? h]
             [d real? 0])
         pict?]{

Creates an arbitrary self-rendering pict.  The arguments to the
rendering procedure will be a drawing context and top-left location for
drawing.

The @racket[w], @racket[h], @racket[a], and @racket[d] arguments 
determine the width, height, ascent, and descent of the
of the resulting pict's @tech{bounding box} respectively.

When the rendering procedure is called, the current pen and brush will
be @racket['solid] and in the pict's color and @racket[linewidth], and the scale and
offset of the drawing context will be set. The text mode will be transparent, but
the font and text colors are not guaranteed to be anything in
particular.

@examples[#:eval ss-eval
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush
          (new brush% [style 'fdiagonal-hatch]
                      [color "darkslategray"]))
        (send dc set-pen
          (new pen% [width 3] [color "slategray"]))
        (define path (new dc-path%))
        (send path move-to 0 0)
        (send path line-to 50 0)
        (send path line-to 25 50)
        (send path close)
        (send dc draw-path path dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen old-pen))
    50 50)]

The @racket[draw] is called during the dynamic extent of
the call to @racket[dc] as part of the contract checking.

Specifically, the pre-condition portion of the contract
for @racket[dc] concocts a @racket[dc<%>] object with a 
random initial state, calls the @racket[draw] argument 
with that @racket[dc<%>] and then checks to make sure that 
@racket[draw] the state of the  @racket[dc<%>] object
is the same as it was before @racket[draw] was called.

@examples[#:eval 
          ss-eval
          (dc (λ (dc dx dy)
                (send dc set-brush "red" 'solid)
                (send dc set-pen "black" 1 'transparent)
                (send dc draw-ellipse dx dy 50 50))
              50 50)]

@history[#:changed "1.3" @list{The @racket[draw] argument is 
                               now called by the @racket[#:pre] 
                               condition of @racket[dc].}]
}

@defproc[(unsafe-dc [draw (-> (is-a?/c dc<%>) real? real? any)]
                    [w real?]
                    [h real?]
                    [a real? h]
                    [d real? 0])
         pict?]{
 Like @racket[dc], except that the @racket[draw] procedure is
 not called during the precondition contract checking.
}

@defproc*[([(blank [size real? 0]) pict?]
           [(blank [w real?] [h real?]) pict?]
           [(blank [w real?] [a real?] [d real?]) pict?]
           [(blank [w real?] [h real?] [a real?] [d real?]) pict?])]{

Creates a pict that draws nothing. The one-argument case supplies a
value used for both the width and height of the resulting pict's
@tech{bounding box}. In the one- and two-argument
case, the ascent and descent are @math{0} for the resulting pict's
bounding box; in the three-argument case, the height is computed by
adding the given ascent and descent.

@examples[#:eval ss-eval
  (blank 50)
]}


@defproc[(text [content string?]
               [style text-style/c null]
               [size (integer-in 1 1024) 12]
               [angle real? 0])
         pict?]{

Creates a pict that draws text. For creating text picts within a slide
presentation, see @racket[t]. The size of the resulting pict may
depend on the value of @racket[dc-for-text-size].

The @racket[style] argument must be one of the following:

@itemize[

 @item{@racket[null] --- the default, same as @racket['default]}

 @item{a @racket[font%] object}

 @item{a font family symbol, such a @racket['roman] (see @racket[font%])}

 @item{a font face string, such as @racket["Helvetica"] (see @racket[font%])}

 @item{@racket[(cons _str _sym)] combining a face string and a font
       family (in case the face is unavailable; see @racket[font%])}

 @item{@racket[(cons 'bold style)] for a valid @racket[style]}

 @item{@racket[(cons 'italic style)]}
 @item{@racket[(cons 'subscript style)]}
 @item{@racket[(cons 'superscript style)]}

@item{@racket[(cons 'large-script style)] --- makes subscripts and
      superscripts larger, which is more suitable for small text sizes
      as might appear in print

      @history[#:added "1.5"]}

@item{@racket[(cons 'caps style)]}

 @item{@racket[(cons 'combine style)] --- allows kerning and ligatures
      (the default, unless the @racket['modern] family is specified)}

 @item{@racket[(cons 'no-combine style)] --- renders characters individually}

 @item{@racket[(cons 'aligned style)] --- enables hinting, which
       rounds metrics to integers}

 @item{@racket[(cons 'unaligned style)] --- disables hinting (which is
       the default), so that metrics are scalable}

 @item{@racket[(cons _color style)] --- where @racket[_color] is a @racket[color%] object,
       @racket[colorize]s the text}

]

If both @racket['combine] and @racket['no-combine] are specified, the
first one in @racket[style] takes precedence. Similarly, if both
@racket['aligned] and @racket['unaligned] are specified, the first one
in @racket[style] takes precedence. If @racket['caps] is specified,
the @racket[angle] must be zero.

The given @racket[size] is in pixels, but it is ignored if a
@racket[font%] object is provided in the text-style.

The @racket[angle] is in radians, and positive values rotate
counter-clockwise. For a non-zero @racket[angle], the resulting
pict's @tech{bounding box} covers the rotated text, and the descent is zero
and the ascent is the height.

@examples[#:eval ss-eval
  (text "tom collins")
  (text "g & t" (cons 'bold 'roman))
  (text "martini" null 13 (/ pi 2))
]}


@defproc*[([(hline [w real?] [h real?]
                   [#:segment seg-length (or/c #f real?) #f]) pict?]
           [(vline [w real?] [h real?]
                   [#:segment seg-length (or/c #f real?) #f]) pict?])]{

Straight lines, centered within their @tech{bounding box}es.

@examples[#:eval ss-eval
  (hline 40 5)
  (hline 40 30)
  (vline 5 40 #:segment 5)
]}


@defproc[(frame [pict pict-convertible?]
                [#:segment seg-length (or/c #f real?) #f]
                [#:color color (or/c #f string? (is-a?/c color%)) #f]
                [#:line-width width (or/c #f real?) #f])
          pict?]{

Frames a given pict. If the color or line width are provided, the
override settings supplied by the context.

@examples[#:eval ss-eval
  (frame (circle 30))
  (frame (circle 30) #:segment 5)
  (frame (circle 30) #:color "chartreuse" #:line-width 3)
]}

@defproc*[([(ellipse [w real?] [h real?]
                     [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                     [#:border-width border-width (or/c #f real?) #f])
                     pict?]
           [(circle [diameter real?]
                    [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                    [#:border-width border-width (or/c #f real?) #f])
                    pict?]
           [(filled-ellipse [w real?] [h real?] [#:draw-border? draw-border? any/c #t]
                            [#:color color (or/c #f string? (is-a?/c color%)) #f]
                            [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                            [#:border-width border-width (or/c #f real?) #f])
                            pict?]
           [(disk [diameter (and/c rational? (not/c negative?))]
                  [#:draw-border? draw-border? any/c #t]
                  [#:color color (or/c #f string? (is-a?/c color%)) #f]
                  [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                  [#:border-width border-width (or/c #f real?) #f])
                  pict?])]{

Unfilled and filled ellipses.

If @racket[draw-border?] is @racket[#f], then the pen is set to be transparent
before drawing the ellipse.
The @racket[color], @racket[border-color] and @racket[border-width] arguments
control the fill color, color of the border, and width of the border,
respectively.
If these arguments are @racket[#f], values set using @racket[linewidth] and
@racket[colorize] are used instead.
Passing non-@racket[#f] values as @racket[border-color] or @racket[border-width]
when @racket[draw-border?] is @racket[#f] results in a contract violation.

@examples[#:eval ss-eval
  (ellipse 40 30)
  (circle 30)
  (filled-ellipse 30 40)
  (disk 30)
  (disk 40 #:color "Chartreuse" #:border-color "Medium Aquamarine" #:border-width 5)
]

@history[#:changed "1.4" @elem{Added @racket[#:color], @racket[#:border-color] and @racket[#:border-width] arguments.}]{}
}

@defproc*[([(rectangle [w real?] [h real?]
                       [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                       [#:border-width border-width (or/c #f real?) #f])
                       pict?]
           [(filled-rectangle [w real?]
                              [h real?]
                              [#:draw-border? draw-border? any/c #t]
                              [#:color color (or/c #f string? (is-a?/c color%)) #f]
                              [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                              [#:border-width border-width (or/c #f real?) #f])
            pict?])]{

Unfilled and filled rectangles.

If @racket[draw-border?] is @racket[#f], then the pen is set to be transparent
before drawing the rectangle.
The @racket[color], @racket[border-color] and @racket[border-width] arguments
control the fill color, color of the border, and width of the border,
respectively.
If these arguments are @racket[#f], values set using @racket[linewidth] and
@racket[colorize] are used instead.
Passing non-@racket[#f] values as @racket[border-color] or @racket[border-width]
when @racket[draw-border?] is @racket[#f] results in a contract violation.

@examples[#:eval ss-eval
  (rectangle 50 50)
  (filled-rectangle 50 80)
  (filled-rectangle 60 70 #:color "Thistle" #:border-color "Gainsboro" #:border-width 10)
]

@history[#:changed "1.4" @elem{Added @racket[#:color], @racket[#:border-color] and @racket[#:border-width] arguments.}]{}
}

@defproc*[([(rounded-rectangle [w real?] [h real?] 
                               [corner-radius real? -0.25]
                               [#:angle angle real? 0]
                               [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                               [#:border-width border-width (or/c #f real?) #f])
            pict?]
           [(filled-rounded-rectangle [w real?] [h real?]
                                      [corner-radius real? -0.25]
                                      [#:angle angle real? 0] 
                                      [#:draw-border? draw-border? any/c #t]
                                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                                      [#:border-color border-color (or/c #f string? (is-a?/c color%)) #f]
                                      [#:border-width border-width (or/c #f real?) #f])
            pict?])]{

Unfilled and filled rectangles with rounded corners.  The
@racket[corner-radius] is used to determine how much
rounding occurs in the corners. If it is a positive number,
then it determines the radius of a circle touching the edges
in each corner, and the rounding of the rectangle follow the
edge of those circles. If it is a negative number, then the
radius of the circles in the corners is the absolute value of the
@racket[corner-radius] times the smaller of @racket[width]
and @racket[height].

The @racket[angle] determines how much the rectangle is
rotated, in radians.

If @racket[draw-border?] is @racket[#f], then the pen is set to be transparent
before drawing the rectangle.
The @racket[color], @racket[border-color] and @racket[border-width] arguments
control the fill color, color of the border, and width of the border,
respectively.
If these arguments are @racket[#f], values set using @racket[linewidth] and
@racket[colorize] are used instead.
Passing non-@racket[#f] values as @racket[border-color] or @racket[border-width]
when @racket[draw-border?] is @racket[#f] results in a contract violation.

@examples[#:eval ss-eval
  (rounded-rectangle 40 40 -0.3 #:angle (/ pi 4))
  (filled-rounded-rectangle 50 40)
  (filled-rounded-rectangle 70 30 #:color "Burlywood" #:border-color "Bisque" #:border-width 8)
]

@history[#:changed "1.4" @elem{Added @racket[#:color], @racket[#:border-color] and @racket[#:border-width] arguments.}]{}
}

@defproc[(bitmap [img (or/c path-string?
                            (is-a?/c bitmap%)
                            pict-convertible?
                            convertible?)])
         pict]{

Returns a pict that displays a bitmap. When a path is provided, the image is
loaded with the @racket['unknown/mask] flag, which means that a mask
bitmap is generated if the file contains a mask.

If the bitmap cannot be loaded, if the given @racket[bitmap%] object
is not valid, or if the @racket[bitmap-draft-mode] parameter is set to
@racket[#t], the result pict draws the words ``bitmap failed''.

 If @racket[img] is both @tech[#:doc '(lib "file/scribblings/file.scrbl")]{convertible}
 and @tech{pict convertible}, then the pict conversion is used. If both
 apply, the pict conversion is used. If pict conversion is used, the
 pict is drawn into a bitmap and the result of @racket[bitmap] draws
 that bitmap. If
 @tech[#:doc '(lib "file/scribblings/file.scrbl")]{convertible} is used,
 it is used with the @racket['png-bytes] conversion mode.
}


@defproc*[([(arrow [size real?] [radians real?]) pict?]
           [(arrowhead [size real?] [radians real?]) pict?])]{

Creates an arrow or arrowhead in the specific direction within a
@racket[size] by @racket[size] pict. Points on the arrow may extend
slightly beyond the @tech{bounding box}.

@examples[#:eval ss-eval
  (arrow 30 0)
  (arrow 30 (/ pi 2))
  (arrowhead 30 0)
]}


@defproc*[([(pip-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrow-line [dx real?] [dy real?] [size real?]) pict?]
           [(pip-arrows-line [dx real?] [dy real?] [size real?]) pict?])]{

Creates a line (with some number of arrowheads) as a zero-sized pict
suitable for use with @racket[pin-over]. The 0-sized picture contains
the starting point.

The @racket[size] is used for the arrowhead size. Even though
@racket[pip-line] creates no arrowheads, it accepts the @racket[size]
argument for consistency with the other functions.}

@defproc*[([(pin-line [pict pict-convertible?]
                      [src pict-path?]
                      [find-src (pict-convertible? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict-convertible? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:alpha alpha (real-in 0.0 1.0) #f]
                      [#:style style (or/c 'transparent 'solid 'xor 'hilite
                                           'dot 'long-dash 'short-dash 'dot-dash
                                           'xor-dot 'xor-long-dash 'xor-short-dash
                                           'xor-dot-dash #f) #f]
                      [#:under? under? any/c #f]
                      [#:label label pict? (blank)]
                      [#:x-adjust-label x-adjust-label real? 0]
                      [#:y-adjust-label y-adjust-label real? 0])
            pict?]
           [(pin-arrow-line [arrow-size real?] [pict pict-convertible?]
                      [src pict-path?]
                      [find-src (pict-convertible? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict-convertible? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:alpha alpha (real-in 0.0 1.0) #f]
                      [#:style style (or/c 'transparent 'solid 'xor 'hilite
                                           'dot 'long-dash 'short-dash 'dot-dash
                                           'xor-dot 'xor-long-dash 'xor-short-dash
                                           'xor-dot-dash #f)
                               #f]
                      [#:under? under? any/c #f]
                      [#:label label pict? (blank)]
                      [#:x-adjust-label x-adjust-label real? 0]
                      [#:y-adjust-label y-adjust-label real? 0]
                      [#:solid? solid? any/c #t]
		      [#:hide-arrowhead? hide-arrowhead? any/c #f])
            pict?]
           [(pin-arrows-line [arrow-size real?] [pict pict-convertible?]
                      [src pict-path?]
                      [find-src (pict-convertible? pict-path? . -> . (values real? real?))]
                      [dest pict-path?]
                      [find-dest (pict-convertible? pict-path? . -> . (values real? real?))]
                      [#:start-angle start-angle (or/c real? #f) #f]
                      [#:end-angle end-angle (or/c real? #f) #f]
                      [#:start-pull start-pull real? 1/4]
                      [#:end-pull end-pull real? 1/4]
                      [#:line-width line-width (or/c #f real?) #f]
                      [#:color color (or/c #f string? (is-a?/c color%)) #f]
                      [#:alpha alpha (real-in 0.0 1.0) #f]
                      [#:style style (or/c 'transparent 'solid 'xor 'hilite
                                           'dot 'long-dash 'short-dash 'dot-dash
                                           'xor-dot 'xor-long-dash 'xor-short-dash
                                           'xor-dot-dash #f)
                               #f]
                      [#:under? under? any/c #f]
                      [#:label label pict-convertible? (blank)]
                      [#:x-adjust-label x-adjust-label real? 0]
                      [#:y-adjust-label y-adjust-label real? 0]
                      [#:solid? solid? any/c #t]
		      [#:hide-arrowhead? hide-arrowhead? any/c #f])
            pict?])]{

Adds a line or line-with-arrows onto @racket[pict], using one of the
pict-finding functions (e.g., @racket[lt-find]) to extract the source
and destination of the line.

If @racket[under?] is true, then the line and arrows are added under
the existing @racket[pict] drawing, instead of on top. If
@racket[solid?] is false, then the arrowheads are hollow instead of
filled.

The @racket[start-angle], @racket[end-angle], @racket[start-pull], and
@racket[end-pull] arguments control the curve of the line (and the
defaults produce a straight line):

@itemize[

 @item{The @racket[start-angle] and @racket[end-angle] arguments
       specify the direction of curve at its start and end positions;
       if either is @racket[#f], it defaults to the angle of a
       straight line from the start position to end position.}

 @item{The @racket[start-pull] and @racket[end-pull] arguments specify
       a kind of momentum for the starting and ending angles; larger
       values preserve the angle longer.}

]

The @racket[line-width], @racket[color], @racket[alpha], and @racket[style] arguments
apply to the added line.

When the @racket[hide-arrowhead?] argument is a true value, then space
for an arrowhead is kept around the line, but the arrowhead itself is
not drawn.

When the @racket[label] argument is non-false, the given pict is used as a
label for the line, and moved by (@racket[x-adjust-label], @racket[y-adjust-label]).

@defexamples[#:eval ss-eval
  (define pict-a (rectangle 40 40))
  (define pict-b (circle 40))
  (define combined (hc-append 200 pict-a pict-b))
  (pin-line combined
            pict-a cc-find
            pict-b cc-find)
  (pin-arrow-line 30 combined
                  pict-a rc-find
                  pict-b lc-find
                  #:line-width 3
                  #:style 'long-dash
                  #:color "medium goldenrod"
                  #:label (text "From Square to Circle"))
  (pin-arrows-line 30 combined
                   pict-a rc-find
                   pict-b lc-find
                   #:start-angle (/ pi 11)
                   #:end-angle (- (/ pi 11))
                   #:solid? #f)
]

@history[#:changed "1.4" @elem{Added @racket[#:label], @racket[#:x-adjust-label] and @racket[#:y-adjust-label] arguments.}]{}
}


@defthing[text-style/c contract?]{

A contract that matches the second argument of @racket[text].}

@defboolparam[bitmap-draft-mode on?]{

A parameter that determines whether @racket[bitmap] loads/uses a
bitmap.}


@; ------------------------------------------------------------------------

@section{Pict Combiners}

@defproc*[([(vl-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(vc-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(vr-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(ht-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(htl-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(hc-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(hbl-append [d real? 0.0] [pict pict-convertible?] ...) pict?]
           [(hb-append [d real? 0.0] [pict pict-convertible?] ...) pict?])]{

Creates a new pict as a column (for @racket[v...-append]) or row (for
@racket[h...-append]) of other picts. The optional @racket[d] argument
specifies amount of space to insert between each pair of pictures in
making the column or row.

Different procedures align pictures in the orthogonal direction in
different ways. For example, @racket[vl-append] left-aligns all of the
pictures.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline. If at least one @racket[pict] is supplied,
then the last element (as reported by @racket[pict-last]) for the
result is @racket[(or (pict-last pict) pict)] for the using last
supplied @racket[pict].

@defexamples[#:eval ss-eval
  (define combiners (list vl-append vc-append vr-append
                          ht-append htl-append hc-append
                          hbl-append hb-append))
  (define names (list "vl-append" "vc-append" "vr-append"
                      "ht-append" "htl-append" "hc-append"
                      "hbl-append" "hb-append"))
  (define pict-a (colorize (filled-rectangle 60 30) "tomato"))
  (define pict-b (colorize (disk 45) "cornflower blue"))
  (define picts
    (for/list ([combiner combiners] [name names])
      (list (text name null 15)
            (combiner pict-a pict-b))))
  (take picts 4)
  (drop picts 4)
]}

@defproc*[([(lt-superimpose [pict pict-convertible?] ...) pict?]
           [(ltl-superimpose [pict pict-convertible?] ...) pict?]
           [(lc-superimpose [pict pict-convertible?] ...) pict?]
           [(lbl-superimpose [pict pict-convertible?] ...) pict?]
           [(lb-superimpose [pict pict-convertible?] ...) pict?]
           [(ct-superimpose [pict pict-convertible?] ...) pict?]
           [(ctl-superimpose [pict pict-convertible?] ...) pict?]
           [(cc-superimpose [pict pict-convertible?] ...) pict?]
           [(cbl-superimpose [pict pict-convertible?] ...) pict?]
           [(cb-superimpose [pict pict-convertible?] ...) pict?]
           [(rt-superimpose [pict pict-convertible?] ...) pict?]
           [(rtl-superimpose [pict pict-convertible?] ...) pict?]
           [(rc-superimpose [pict pict-convertible?] ...) pict?]
           [(rbl-superimpose [pict pict-convertible?] ...) pict?]
           [(rb-superimpose [pict pict-convertible?] ...) pict?])]{

Creates a new picture by superimposing a set of pictures. The name
prefixes are alignment indicators: horizontal alignment then vertical
alignment.

The descent of the result corresponds to baseline that is lowest in
the result among all of the picts' descent-specified baselines;
similarly, the ascent of the result corresponds to the highest
ascent-specified baseline. The last element (as reported by
@racket[pict-last]) for the result is the lowest, right-most among the
last-element picts of the @racket[pict] arguments, as determined by
comparing the last-element bottom-right corners.

@defexamples[#:eval ss-eval
  (define combiners (list lt-superimpose  ltl-superimpose lc-superimpose
                          lbl-superimpose lb-superimpose  ct-superimpose
                          ctl-superimpose cc-superimpose  cbl-superimpose
                          cb-superimpose  rt-superimpose  rtl-superimpose
                          rc-superimpose  rbl-superimpose rb-superimpose))
  (define names (list "lt-superimpose"  "ltl-superimpose" "lc-superimpose"
                      "lbl-superimpose" "lb-superimpose"  "ct-superimpose"
                      "ctl-superimpose" "cc-superimpose"  "cbl-superimpose"
                      "cb-superimpose"  "rt-superimpose"  "rtl-superimpose"
                      "rc-superimpose"  "rbl-superimpose" "rb-superimpose"))
  (define pict-a (colorize (filled-rectangle 60 30) "tomato"))
  (define pict-b (colorize (disk 45) "cornflower blue"))
  (define picts
    (for/list ([combiner combiners] [name names])
      (list (text name null 15)
            (combiner pict-a pict-b))))
  (take picts 3)
  (take (drop picts 3) 3)
  (take (drop picts 6) 3)
  (take (drop picts 9) 3)
  (take (drop picts 12) 3)
]}


@defproc*[([(pin-over [base pict-convertible?] [dx real?] [dy real?] [pict pict-convertible?])
            pict?]
           [(pin-over [base pict-convertible?]
                      [find-pict pict-path?]
                      [find (pict-convertible? pict-path? . -> . (values real? real?))]
                      [pict pict-convertible?])
            pict?])]{

Creates a pict with the same @tech{bounding box}, ascent, and descent as
@racket[base], but with @racket[pict] placed on top.  The @racket[dx]
and @racket[dy] arguments specify how far right and down the second
pict's corner is from the first pict's corner.  Alternately, the
@racket[find-pict] and @racket[find] arguments find a point in
@racket[base] for @racket[find-pict]; the @racket[find] procedure
should be something like @racket[lt-find].

@examples[#:eval ss-eval
  (pin-over (colorize (filled-rectangle 70 40) "chocolate")
            10 10
            (colorize (filled-rectangle 30 30) "orange"))
  (define top (colorize (filled-rectangle 70 40) "royalblue"))
  (pin-over (vc-append top (colorize (filled-rectangle 70 40) "firebrick"))
            top
            cb-find
            (colorize (disk 20) "white"))
]}


@defproc*[([(pin-under [base pict-convertible?] [dx real?] [dy real?] [pict pict-convertible?])
            pict?]
           [(pin-under [base pict-convertible?] 
                       [find-pict pict-convertible?]
                       [find (pict-convertible? pict-path? . -> . (values real? real?))]
                       [pict pict-convertible?])
            pict?])]{

Like @racket[pin-over], but @racket[pict] is drawn before
@racket[base] in the resulting combination.

@examples[#:eval ss-eval
  (define txt
    (colorize (text "P I C T S" null 25) "chocolate"))
  (define rect
    (colorize
     (filled-rectangle (pict-width txt) (* 0.3 (pict-height txt)))
     "lemonchiffon"))
  (pin-under txt
             0
             (- (/ (pict-height txt) 2)
                (/ (pict-height rect) 2))
             rect)
]}


@defproc[(table [ncols exact-positive-integer?]
                [picts (non-empty-listof pict-convertible?)]
                [col-aligns (or/c (list*of (->* () #:rest (listof pict-convertible?) pict-convertible?))
                                  (listof (->* () #:rest (listof pict-convertible?) pict-convertible?)))]
                [row-aligns (or/c (list*of (->* () #:rest (listof pict-convertible?) pict-convertible?))
                                  (listof (->* () #:rest (listof pict-convertible?) pict-convertible?)))]
                [col-seps (or/c (list*of real?) (listof real?))]
                [row-seps (or/c (list*of real?) (listof real?))])
         pict?]{

Creates a table given a list of picts. The @racket[picts] list is a
concatenation of the table's rows (which means that a Racket
@racket[list] call can be formatted to reflect the shape of the output
table).
  
The @racket[col-aligns], @racket[row-aligns], @racket[col-seps], and
@racket[row-seps] arguments are ``lists'' specifying the row and
columns alignments separation between rows and columns.  For @math{c}
columns and @math{r} rows, the first two should have @math{c} and
@math{r} superimpose procedures, and the last two should have
@math{c-1} and @math{r-1} numbers, respectively. The lists can be
``improper'' (i.e., ending in a number instead of an empty list), in
which case the non-pair @racket[cdr] is used as the value for all remaining
list items that were expected. The @racket[col-aligns] and
@racket[row-aligns] procedures are used to superimpose all of the
cells in a column or row; this superimposition determines the total
width or height of the column or row, and also determines the
horizontal or vertical placement of each cell in the column or row.

@defexamples[#:eval 
             ss-eval
             (table 4
                    (map (λ (x) (text (format "~a" x)))
                         (list 1 2 3 4
                               5 6 7 8
                               9000 10 11 12))
                    cc-superimpose
                    cc-superimpose
                    10
                    10)
             
             (table 4
                    (map (λ (x) (text (format "~a" x)))
                         (list 1 2 3 4
                               5 6 7 8
                               9000 10 11 12))
                    rc-superimpose
                    cc-superimpose
                    10
                    10)]
}

@; ------------------------------------------------------------------------

@section{Pict Drawing Adjusters}

@defproc*[([(scale [pict pict-convertible?] [factor real?]) pict?]
           [(scale [pict pict-convertible?] [w-factor real?] [h-factor real?]) pict?])]{

Scales a pict drawing, as well as its @tech{bounding box}, by multiplying
it current size by @racket[factor] (if two arguments are supplied)
or by multiplying the current width by @racket[w-factor] and current height by
@racket[h-factor] (if three arguments are supplied).

The drawing is scaled by adjusting the destination @racket[dc<%>]'s
scale while drawing the original @racket[pict].

@examples[#:eval
          ss-eval
          (filled-rectangle 40 40)
          (scale (filled-rectangle 40 40) 1.5)
          (scale (filled-rectangle 40 40) 2 1.5)]

}

@defproc*[([(scale-to-fit [pict pict-convertible?] [size-pict pict-convertible?]
                          [#:mode mode (or/c 'preserve 'inset
                                             'preserve/max 'inset/max
                                             'distort)
                           'preserve])
                          pict?]
           [(scale-to-fit [pict pict-convertible?] [width real?] [height real?]
                          [#:mode mode (or/c 'preserve 'inset
                                             'preserve/max 'inset/max
                                             'distort)
                           'preserve])
                          pict?])]{
  Scales @racket[pict] so that it fits within the bounding box of
         @racket[size-pict] (if two arguments are supplied) or
         into a box of size @racket[width] by @racket[height] 
         (if three arguments are supplied).

 If @racket[mode] is @racket['preserve] or
 @racket['preserve/max], the width and height are scaled by
 the same factor so @racket[pict]'s aspect ratio is
 preserved. If @racket[mode] is @racket['preserve] the
 result's bounding box will not be larger than @racket[width]
 by @racket[height] but it may be smaller. When @racket[mode]
 is @racket['preserve/max], the opposite is true; the
 bounding box will never be smaller, but might be larger.

 If @racket[mode] is @racket['inset] or @racket['inset/max],
 the aspect ratio is preserved as with @racket['preserve] and
 @racket['preserve/max], but the resulting pict is centered
 on a bounding box of exactly @racket[width] by
 @racket[height].

 If @racket[mode] is @racket['distort], the width and height
 are scaled separately.

@examples[#:eval
          ss-eval
          (define rect (colorize (filled-rectangle 40 40) "olive"))
          rect
          (scale-to-fit rect (disk 60))
          (scale-to-fit rect 80 30 #:mode 'preserve)
          (frame (scale-to-fit rect 80 30 #:mode 'inset))
          (scale-to-fit rect 80 30 #:mode 'preserve/max)
          (cc-superimpose (blank 100 100)
                          (frame (scale-to-fit rect 80 30 #:mode 'inset/max)))
          (scale-to-fit rect 80 30 #:mode 'distort)]

@history[#:changed "1.4" @elem{Added @racket[#:mode] argument.}]{}
}


@defproc[(rotate [pict pict-convertible?] [theta real?]) pict?]{

Rotates a pict's drawing by @racket[theta] radians counter-clockwise.

The @tech{bounding box} of the resulting pict is the box encloses the rotated
corners of @racket[pict] (which inflates the area of the bounding
box, unless @racket[theta] is a multiple of half of @racket[pi]). The
ascent and descent lines of the result's bounding box are the
horizontal lines that bisect the rotated original lines; if the ascent
line drops below the descent line, the two lines are flipped.

@examples[#:eval ss-eval
          (rotate (colorize (filled-rectangle 30 30)
                            "chartreuse")
                  (/ pi 3))]
}


@defproc[(ghost [pict pict-convertible?]) pict?]{

Creates a container picture that doesn't draw the child picture,
but uses the child's size.

@examples[#:eval ss-eval
          (frame (hc-append (ghost (filled-rectangle 30 30))
                            (colorize (disk 30) "turquoise")))]
}


@defproc[(linewidth [w (or/c real? #f)] [pict pict-convertible?]) pict?]{

Selects a specific pen width for drawing, which applies to pen drawing
for @racket[pict] that does not already use a specific pen width.
A @racket[#f] value for @racket[w] makes the pen transparent (in contrast
to a zero value, which means ``as thin as possible for the target device'').

@examples[#:eval ss-eval
          (linewidth 3 (hline 40 1))
          (linewidth 5 (hline 40 1))]
}


@defproc[(linestyle [style (or/c 'transparent 'solid 'xor 'hilite
                                 'dot 'long-dash 'short-dash 'dot-dash
                                 'xor-dot 'xor-long-dash 'xor-short-dash
                                 'xor-dot-dash)]
                    [pict pict-convertible?])
         pict?]{

Selects a specific pen style for drawing, which applies to pen drawing
for @racket[pict] that does not already use a specific pen style.

@examples[#:eval ss-eval
          (define styles
            '(transparent solid xor hilite dot long-dash short-dash
              dot-dash xor-dot xor-long-dash xor-short-dash xor-dot-dash))
          (apply ht-append
                 10
                 (for/list ([style (in-list styles)])
                   (vc-append 5
                     (text (symbol->string style))
                     (linewidth 3 (linestyle style (hline 40 1))))))
]}


@defproc[(colorize [pict pict-convertible?]
                   [color (or/c string? (is-a?/c color%)
                                (list/c byte? byte? byte?))])
         pict?]{

Selects a specific color drawing, which applies to drawing in
@racket[pict] that does not already use a specific color. The
@racket[black-and-white] parameter causes all non-white colors to be
converted to black.

@examples[#:eval ss-eval
  (colorize (disk 40) "lavender")
  (colorize (filled-rectangle 40 40)
            (list #xff #x99 #x55))
  (colorize (arrow 40 0)
            (make-color 170 180 120))
]}

@defproc[(cellophane [pict pict-convertible?] [opacity (real-in 0 1)])
         pict?]{

Makes the given @racket[pict] semi-transparent, where an opacity of
@racket[0] is fully transparent, and an opacity of @racket[1] is fully
opaque.  See @method[dc<%> set-alpha] for information about the
contexts and cases when semi-transparent drawing works.

@examples[#:eval ss-eval
  (cc-superimpose (filled-rectangle 70 45 #:color "darkcyan")
                  (cellophane (disk 40) 0.2))
  (cc-superimpose (filled-rectangle 70 45 #:color "darkcyan")
                  (cellophane (disk 40) 0.8))
]}

@defproc[(clip [pict pict-convertible?]) pict]{

Clips a pict's drawing to its @tech{bounding box}.

@examples[#:eval ss-eval
  (define shape
    (inset (colorize (filled-rectangle 40 40) "thistle") -10))
  shape
  (clip shape)
]}


@defproc*[([(inset/clip [pict pict-convertible?] [amt real?]) pict?]
           [(inset/clip [pict pict-convertible?] [h-amt real?] [v-amt real?]) pict?]
           [(inset/clip [pict pict-convertible?] [l-amt real?] [t-amt real?] 
                        [r-amt real?] [b-amt real?]) pict?])]{

Insets and clips the pict's drawing to its @tech{bounding
box}. Usually, the inset amounts are negative.

@examples[#:eval ss-eval
  (filled-rectangle 40 40 #:color "forestgreen")
  (inset/clip (filled-rectangle 40 40 #:color "forestgreen") -10)
  (inset/clip (filled-rectangle 40 40 #:color "forestgreen")
              -10 -5)
  (inset/clip (filled-rectangle 40 40 #:color "forestgreen")
              -2 -4 -8 -16)
]}


@defform*[[(scale/improve-new-text pict-expr scale-expr)
           (scale/improve-new-text pict-expr x-scale-expr y-scale-expr)]]{

Like the @racket[scale] procedure, but also sets
@racket[current-expected-text-scale] while evaluating @racket[pict-expr].

@examples[#:eval ss-eval
  (text "Hello World" null 25)
  (scale/improve-new-text (text "Hello World" null 25) 2)
  (scale (text "Hello World" null 25) 2)
]}

@defboolparam[black-and-white on?]{

A parameter that determines whether @racket[colorize] uses color or
black-and-white colors.

@examples[#:eval ss-eval
  (colorize (disk 40) "seagreen")
  (parameterize ([black-and-white #t])
    (colorize (disk 40) "seagreen"))
]}

@defproc[(freeze [pict pict-convertible?]) pict?]{
 Creates a bitmap with the same size as @racket[pict], draws
 @racket[pict] into the bitmap, and then returns a pict that
 draws with the bitmap.

 This has the effect of speeding up subsequent drawing of
 the pict and also of cropping it to its bounding box. Any
 sub-picts of @racket[pict] remain intact within the new
 pict.

@examples[#:eval ss-eval
  (define txt
    (colorize (text "Freeze!" null 25) "deepskyblue"))
  (scale txt 2.5)
  (scale (freeze txt) 2.5)
]}


@; ------------------------------------------------------------------------

@section{Bounding Box Adjusters}

@defproc*[([(inset [pict pict-convertible?] [amt real?]) pict?]
           [(inset [pict pict-convertible?] [h-amt real?] [v-amt real?]) pict?]
           [(inset [pict pict-convertible?] [l-amt real?] [t-amt real?] 
                   [r-amt real?] [b-amt real?]) pict?])]{

Extends @racket[pict]'s @tech{bounding box} by adding the given amounts
to the corresponding sides; ascent and descent are extended, too.

@examples[#:eval ss-eval
  (pict-width (disk 40))
  (pict-width (inset (disk 40) -10))
]}


@defproc[(clip-descent [pict pict-convertible?]) pict?]{

Truncates @racket[pict]'s @tech{bounding box} by removing the descent part.

@examples[#:eval ss-eval
  (frame (text "gjy" null 50))
  (frame (clip-descent (text "gjy" null 50)))
]}

@defproc[(clip-ascent [pict pict-convertible?]) pict?]{

Truncates @racket[pict]'s @tech{bounding box} by removing the ascent part.

@examples[#:eval ss-eval
  (frame (text "gjy" null 50))
  (frame (clip-ascent (text "gjy" null 50)))
]}

@defproc[(lift-above-baseline [pict pict-convertible?] [amt real?]) pict?]{

Lifts @racket[pict] relative to its baseline, extending the
@tech{bounding box} height if necessary.

@examples[#:eval ss-eval
  (frame (hbl-append (text "ijijij" null 50)
                     (text "abc" null 50)))
  (frame (hbl-append (lift-above-baseline (text "ijijij" null 50) 20)
                     (text "abc" null 50)))
]}

@defproc[(drop-below-ascent [pict pict-convertible?] [amt real?]) pict?]{

Drops @racket[pict] relative to its ascent line, extending the
@tech{bounding box} height if necessary.

@examples[#:eval ss-eval
  (define txt (text "ijgy" null 50))
  (frame (hbl-append txt (text "abc" null 50)))
  (frame (hbl-append (drop-below-ascent txt 20)
                     (text "abc" null 50)))
]}

@defproc[(baseless [pict pict-convertible?]) pict?]{

Makes the descent @racket[0] and the ascent the same as the height.

@examples[#:eval ss-eval
  (frame (hbl-append (text "gjy" null 50)
                     (text "abc" null 50)))
  (frame (hbl-append (baseless (text "gjy" null 50))
                     (text "abc" null 50)))
]}

@defproc[(refocus [pict pict-convertible?] [sub-pict pict-convertible?]) pict?]{

Assuming that @racket[sub-pict] can be found within @racket[pict],
shifts the overall bounding box to that of @racket[sub-pict] (but
preserving all the drawing of @racket[pict]). The last element, as
reported by @racket[pict-last] is also set to @racket[(or (pict-last
sub-pict) sub-pict)].

@examples[#:eval ss-eval
  (define p1 (filled-rectangle 50 50 #:color "darkkhaki"))
  (define p2 (filled-rectangle 30 30 #:color "sienna"))
  (define combined (cc-superimpose p1 p2))
  combined
  (refocus combined p2)
]}


@defproc[(panorama [pict pict-convertible?]) pict?]{

Shifts the given pict's @tech{bounding box} to enclose the bounding boxes of
all sub-picts (even @racket[launder]ed picts).

@examples[#:eval ss-eval
  (define p1 (filled-rectangle 50 50 #:color "maroon"))
  (define p2 (disk 30 #:color "tomato"))
  (define combined (cc-superimpose p1 p2))
  (refocus combined p2)
  (panorama (refocus combined p2))
]}


@defproc[(use-last [pict pict-convertible?] [sub-pict pict-path?]) pict?]{

Returns a pict like @racket[pict], but with the last element (as
reported by @racket[pict-last]) set to @racket[sub-pict]. The
@racket[sub-pict] must exist as a sub-pict (or path of sub-picts)
within @racket[pict].}

@defproc[(use-last* [pict pict-convertible?] [sub-pict pict-convertible?]) pict?]{

Propagates the last element of @racket[sub-pict] to @racket[pict].

That is, @racket[use-last*] is like @racket[use-last], but the last
element of @racket[sub-pict] is used as the new last element for
@racket[pict], instead of @racket[sub-pict] itself---unless
@racket[(pict-last sub-pict)] is @racket[#f], in which case
@racket[sub-pict] is used as the last element of @racket[pict].}

@; ------------------------------------------------------------------------

@section{Pict Finders}

@defproc*[([(lt-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(ltl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(lc-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(lbl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(lb-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(ct-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(ctl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(cc-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(cbl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(cb-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(rt-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(rtl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(rc-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(rbl-find [pict pict-convertible?] [find pict-path?]) (values real? real?)]
           [(rb-find [pict pict-convertible?] [find pict-path?]) (values real? real?)])]{

Locates a pict designated by @racket[find] is within @racket[pict]. If
@racket[find] is a pict, then the @racket[pict] must have been created
as some combination involving @racket[find].

If @racket[find] is a list, then the first element of @racket[find]
must be within @racket[pict], the second element of @racket[find] must
be within the second element, and so on.

@examples[#:eval ss-eval
  (define p1 (disk 60))
  (define p2 (rectangle 60 60))
  (define p3 (hc-append p1 p2))
  (define p4 (hc-append p3 (arrow 60 0)))
  (lt-find p4 p1)
  (cb-find p4 p2)
  (rb-find p3 p1)
  (lt-find p4 (list p1))
  (lt-find p4 (list p2 p1))
  (lt-find p4 (list p2))
  (lt-find p4 (list p3 p2))
  (pin-over p4 p2 lt-find
            (colorize (text "lt-find") "darkgreen"))
  (panorama
   (pin-over p4 p2 rb-find
             (colorize (text "rb-find") "darkgreen")))
]}

@defproc[(pict-path? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @racket[pict-convertible?] or a non-empty
list of @racket[pict-convertible?]s.

@examples[#:eval ss-eval
  (pict-path? null)
  (pict-path? (disk 30))
  (pict-path? (list (disk 30) (rectangle 10 10)))
]}


@defproc[(launder [pict pict-convertible?]) pict?]{

Creates a pict that has the same drawing and @tech{bounding box} of
@racket[pict], but which hides all of its sub-picts so that they
cannot be found with functions like @racket[lt-find]. If @racket[pict]
has a last-line pict, then the laundered pict has a fresh last-line
pict with the same shape and location.

@examples[#:eval ss-eval
  (define p1 (disk 60))
  (define p2 (rectangle 60 60))
  (define p3 (hc-append p1 p2))
  (lt-find (launder p4) p3)
]}

@; ------------------------------------------------------------------------

@include-section["more.scrbl"]

@; ----------------------------------------

@include-section["anim.scrbl"]

@; ----------------------------------------

@include-section["color.scrbl"]

@; ----------------------------------------

@include-section["shadow.scrbl"]

@; ----------------------------------------

@include-section["conditional.scrbl"]

@; ----------------------------------------

@include-section["tree-layout.scrbl"]

@; ----------------------------------------

@section{Miscellaneous}

@defproc[(hyperlinkize [pict pict-convertible?])
         pict?]{

Adds an underline and blue color. The @racket[pict]'s height and
descent are extended.

@examples[#:eval ss-eval
  (hyperlinkize (text "Help me I'm trapped in this documentation"
                      null 15))
]}


@defproc[(scale-color [factor real?]
                      [color (or/c string (is-a?/c color%))])
         (is-a?/c color%)]{

Scales a color, making it brighter or darker. If the factor is less
than 1, the color is darkened by multiplying the RGB components by the
factor. If the factor is greater than 1, the color is lightened by
dividing the gap between the RGB components and 255 by the factor.

@examples[#:eval ss-eval
  (disk 30 #:color "royalblue")
  (disk 30 #:color (scale-color 0.5 "royalblue"))
  (disk 30 #:color (scale-color 1.5 "royalblue"))
]}

@defproc[(color-series [dc (is-a?/c dc<%>)]
                       [max-step exact-nonnegative-integer?]
                       [step-delta (and/c exact? positive?)]
                       [start (or/c string? (is-a?/c color%))]
                       [end (or/c string? (is-a?/c color%))]
                       [proc (exact? . -> . any)]
                       [set-pen? any/c]
                       [set-brush? any/c])
          void?]{

Calls a @racket[proc] multiple times, gradually changing the pen
and/or brush color for each call. For the first call, the current pen
and/or brush color matches @racket[start]; for the last call, it
matches @racket[end]; and for intermediate calls, the color is an
intermediate color.

The @racket[max-step] and @racket[step-delta] arguments should be
exact numbers; the procedure is called with each number from 0 to
@racket[max-step] inclusive using a @racket[step-delta] increment.}

@; ------------------------------------------------------------------------

@section{Rendering}

@defparam[dc-for-text-size dc (or/c #f (is-a?/c dc<%>))]{

A parameter that is used to determine the @tech{bounding box} of picts
created with @racket[text].

The drawing context installed in this parameter need not be the same
as the ultimate drawing context, but it should measure text in the same
way. Under normal circumstances, font metrics are the same for all
drawing contexts, so the default value of @racket[dc-for-text-size] is
a @racket[bitmap-dc%] that draws to a 1-by-1 bitmap.}


@defparam[convert-bounds-padding padding (list/c (>=/c 0) (>=/c 0) (>=/c 0) (>=/c 0))]{

A parameter that determines an amount of padding added to each edge of
a @tech{pict} when converting to a format like @racket['png@2x-bytes+bounds8]
(see @racketmodname[file/convertible]). The default value of the parameter
is @racket['(3 3 3 3)], which adds three pixels to each edge to accommodate
a small amount of drawing outside the pict's @tech{bounding box}.

@history[#:added "1.2"]}


@defproc[(draw-pict [pict pict-convertible?]
                    [dc (is-a?/c dc<%>)]
                    [x real?]
                    [y real?])
         void?]{

Draws @racket[pict] to @racket[dc], with its top-left corner at offset
 (@racket[x], @racket[y]).}


@defproc[(pict->bitmap [pict pict-convertible?] 
                       [smoothing (or/c 'unsmoothed 'smoothed 'aligned) 'aligned]
                       [#:make-bitmap make-a-bitmap
                                      (exact-positive-integer? exact-positive-integer?
                                       . -> . (is-a?/c bitmap%))
                                      make-bitmap])
         (is-a?/c bitmap%)]{

Returns a @racket[bitmap%] with @racket[pict] drawn on it in the
top-left corner (@racket[0], @racket[0]).
Drawing the pict into the bitmap uses the smoothing mode @racket[smoothing];
see @method[dc<%> set-smoothing] for more information on smoothing modes.

The @racket[make-a-bitmap] function is used to create the bitmap,
giving it the size of @racket[pict] rounded up to integer
dimensions. The default function, @racket[make-bitmap], creates a
bitmap with an alpha channel. Supply @racket[make-screen-bitmap], instead,
to create a bitmap that is consistent with drawing directly to the screen.

@history[#:changed "1.7" @elem{Added the @racket[#:make-bitmap] argument.}]}

@defproc[(pict->argb-pixels [pict pict-convertible?]
                            [smoothing (or/c 'unsmoothed 'smoothed 'aligned) 'aligned])
         bytes?]{
Returns the @racket[bytes?] with the pixels corresponding the bitmap that @racket[pict->bitmap]
returns. Each pixel has four bytes in the result: the alpha, red, green, and blue components.

@examples[#:eval 
          ss-eval
          (pict->argb-pixels
           (filled-rectangle 1 1))
          (pict->argb-pixels
           (colorize (filled-rectangle 1 1) "red"))]

@history[#:added "1.1"]
}

@defproc[(argb-pixels->pict [bytes bytes?] [width exact-nonnegative-integer?]) pict?]{
  Constructs a pict from @racket[bytes] with the width @racket[width]. Each pixel
  in the resulting pict corresponds to four entries in @racket[bytes]: the alpha value,
  and the red, green, and blue values.
  
 @examples[#:eval 
          ss-eval
          (let ([b (make-bytes (* 40 40 4) 255)])
            (for ([x (in-range (bytes-length b))])
              (code:comment "when in one of two vertical bands (10-20 & 30-40)")
              (when (or (<= 10 (modulo (quotient x 4) 40) 20)
                        (<= 30 (modulo (quotient x 4) 40) 40))
                (code:comment "change the red and green fields of the pixel")
                (when (= 1 (modulo x 4)) (bytes-set! b x 0))
                (when (= 2 (modulo x 4)) (bytes-set! b x 150))))
            (argb-pixels->pict b 40))]
 
@history[#:added "1.1"]
}

@defproc[(make-pict-drawer [pict pict-convertible?])
         ((is-a?/c dc<%>) real? real? . -> . void?)]{

Generates a pict-drawer procedure for multiple renderings of
@racket[pict]. Using the generated procedure can be faster than
repeated calls to @racket[draw-pict].}


@defproc[(show-pict [pict pict-convertible?]
                    [w (or/c #f exact-nonnegative-integer?) #f] 
                    [h (or/c #f exact-nonnegative-integer?) #f] 
                    [#:frame-x frame-x (or/c (integer-in -10000 10000) #f)] 
                    [#:frame-y frame-y (or/c (integer-in -10000 10000) #f)] 
                    [#:frame-style frame-style (listof (or/c 'no-resize-border 'no-caption
                                                             'no-system-menu 'hide-menu-bar
                                                             'toolbar-button 'float 'metal))])
         void?]{

Opens a frame that displays @racket[pict].  The frame adds one method,
@racket[set-pict], which takes a pict to display. The optional
@racket[w] and @racket[h] arguments specify a minimum size for the
frame's drawing area, and the @racket[frame-x], @racket[frame-y], 
and @racket[frame-style] keyword arguments behave in the same manner as @racket[x], 
@racket[y], and @racket[style] arguments for the @racket[frame%].}

@defparam[current-expected-text-scale scales (list/c real? real?)]{

A parameter used to refine text measurements to better match an
expected scaling of the image. The @racket[scale/improve-new-text]
form sets this parameter while also scaling the resulting pict.}

@;----------------------------------------

@section{Conversion to Picts}

@defmodule[pict/convert]{The
@racketmodname[pict/convert] library defines a protocol for
values to be @deftech{pict convertible}, meaning they can convert themselves to @tech{picts}.
The protocol is used by DrRacket's interactions window, for example, to render
values that it prints. Anything that is @racket[pict-convertible?]
can be used wherever a @racket[pict] can be used. These values will
be automatically converted to a pict when needed.}

@defthing[prop:pict-convertible struct-type-property?]{

A property whose value should be a procedure matching the
contract @racket[(-> any/c pict?)]. The
procedure is called when a structure with the property is passed to
@racket[pict-convert]; the argument to the procedure is the
structure, and the procedure's result should be a pict.
}

@defthing[prop:pict-convertible? struct-type-property?]{
A property whose value should be a predicate procedure
(i.e., matching the contract @racket[predicate/c]).

If this property is not set, then it is assumed to be
the function @racket[(λ (x) #t)]. 

If this property is set, then this procedure is called
by @racket[pict-convertible?] to determine if this particular
value is convertible (thereby supporting situations
where some instances of a given value are convertible
to picts, but others are not).
}

@defproc[(pict-convertible? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] are @tech{pict convertible}
(by having the @racket[prop:pict-convertible] property)
and @racket[#f] otherwise. This function returns @racket[#t] for @racket[pict?]s.
}

@defproc[(pict-convert [v pict-convertible?]) pict?]{
  Requests a data conversion from @racket[v] to a pict.
}

@(close-eval ss-eval)
