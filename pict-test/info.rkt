#lang info

(define collection 'multi)

(define deps '("base"))

(define pkg-desc "tests for \"pict-lib\"")

(define pkg-authors '())
(define build-deps '("pict-lib"
                     "rackunit-lib"))
(define update-implies '("pict-lib"))
