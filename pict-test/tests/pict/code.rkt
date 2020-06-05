#lang racket
(require pict
         rackunit
         (for-syntax syntax/parse)
         pict/code)

(unless (equal? (pict-width                                         (code a b))
                (pict-width (code a b)))
  (error "indentation seems to affect a `code` pict's width"))


(define-syntax ccode
  (syntax-parser
    [(_ a b ...)
     #`(#,'code #,(syntax->datum #'a) b ...)]))

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))
(check-not-exn
 (lambda () (eval #'(ccode a b c) ns)))


;; Test error handling for code:comment
(check-exn (λ (e)
             (regexp-match? #rx"code:comment.*string\\?" (exn-message e)))
           (λ () (code (code:comment 3))))


;; codeblock-pict

;; test by rendering to a record-dc, and checking the resulting list
;;   of commands
;; Note: this test failing does not imply that the code is broken
;;   changes to the record-dc format (in response to changes in dc),
;;   for example, would break it
;;   if the test fails, look at the picture, and if it looks fine,
;;   then update the expected hash
(require racket/class racket/draw file/md5 rackunit)
(define (test-codeblock-pict-hash s h)
  (define rdc (new record-dc%))
  (draw-pict (codeblock-pict s) rdc 0 0)
  (define commands (send rdc get-recorded-datum))
  ;; commands may include literal floating-point numbers
  ;; of course, this means that different machines, with different FPUs, may
  ;; compute different results, which changes the hash
  ;; KAAAAHAAAAAAAAAAAN!
  (define rounded-commands
    (let loop ([commands commands])
      (cond [(list? commands)
             (map loop commands)]
            [(flonum? commands)
             ;; not even rounding is enough to paper over the differences
             ;; just give up and consider all floats equivalent
             'float]
            [else
             commands])))
  (define hash (md5 (format "~s" rounded-commands)))
  (check-equal? hash h))

(define example
  #<<END
#lang 2d racket
(require 2d/cond)

(define (same? a b)
  #2dcond
  ╔═════════════╦═══════════════════════╦═════════════╗
  ║             ║       (pair? a)       ║ (number? a) ║
  ╠═════════════╬═══════════════════════╬═════════════╣
  ║ (pair? b)   ║ (and (same? (car a)   ║     #f      ║
  ║             ║             (car b))  ║             ║
  ║             ║      (same? (cdr a)   ║             ║
  ║             ║             (cdr b))) ║             ║
  ╠═════════════╬═══════════════════════╬═════════════╣
  ║ (number? b) ║          #f           ║   (= a b)   ║
  ╚═════════════╩═══════════════════════╩═════════════╝)
END
)
(test-codeblock-pict-hash example #"7b91788c2f1dfdf63e7c7fa48f13bb28")

;; make sure that whitespace before #lang doesn't blow up
(define example2
 #<<>>
 #lang racket
1
>>
)
(check-not-exn
 (λ () (test-codeblock-pict-hash example2 #"48281e84c3d32bb488e323d17ed1a0fd")))

;; windows newlines should work
(define example3 "#lang racket\r\n(define x 2)\r\nx")
(test-codeblock-pict-hash example3 #"b32d6e6f8f33dd9a79a6846fede88246")

;; ascent should not be zero for a single line
(define example4 (codeblock-pict #:keep-lang-line? #f "#lang racket\n(define foo 42)"))
(check-pred (λ (v) (> v 0)) (pict-ascent example4))
