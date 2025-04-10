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
(define (test-pict-hash p h)
  (define rdc (new record-dc%))
  (draw-pict p rdc 0 0)
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
(test-pict-hash (codeblock-pict example) #"0799f59e11c86bea943f9ba7cb914978")

;; make sure that whitespace before #lang doesn't blow up
(define example2
 #<<>>
 #lang racket
1
>>
)
(check-not-exn
 (λ () (test-pict-hash (codeblock-pict example2) #"62ec308dd6bed21018107ea44aae18dc")))

;; windows newlines should work
(define example3 "#lang racket\r\n(define x 2)\r\nx")
(test-pict-hash (codeblock-pict example3) #"928d024d7811fb97952f1bad0ab97371")

;; ascent should not be zero for a single line
(define example4 (codeblock-pict #:keep-lang-line? #f "#lang racket\n(define foo 42)"))
(check-pred (λ (v) (> v 0)) (pict-ascent example4))

(test-pict-hash
 (typeset-code
  #'(begin (code:comment "single comment")
           (code:comment2 "double comment")
           (void)))
 #"537157490dcb25174111b1b7045ee035")
