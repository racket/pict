#lang info

(define collection 'multi)

(define deps '("scheme-lib"
               "base"
               "compatibility-lib"
               "draw-lib"
               "syntax-color-lib"))
(define build-deps '("rackunit-lib"))

(define pkg-desc "implementation (no documentation) part of \"pict\"")

(define pkg-authors '(mflatt robby))

(define version "1.13")

(define license
  '(Apache-2.0 OR MIT))
