
(require-library "refer.ss")

(require-library "mrpicts.ss" "texpict")

(begin-elaboration-time
 (require-library "invoke.ss"))

(define-values/invoke-unit/sig mrpict^
  (require-library-unit/sig "mrpictr.ss" "texpict")
  #f
  mred^)

