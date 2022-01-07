#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"pict-lib\"")

(define pkg-authors '(mflatt robby stamourv "spencer@florence.io"))
(define build-deps '("pict-lib"
                     "rackunit-lib"
                     "htdp-lib"
                     ["draw-lib" #:version "1.19"]))
(define update-implies '("pict-lib"))

(define license
  '(Apache-2.0 OR MIT))
