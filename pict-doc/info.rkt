#lang info

(define collection 'multi)

(define build-deps '("mzscheme-doc"
                     "draw-doc"
                     "gui-doc"
                     "slideshow-doc"
                     "draw-lib"
                     "gui-lib"
                     "scribble-doc"
                     "scribble-lib"
                     "slideshow-lib"
                     "pict-lib"
                     "racket-doc"))
(define deps '("base"))
(define update-implies '("pict-lib"))

(define pkg-desc "documentation part of \"pict\"")

(define pkg-authors '(mflatt robby))
