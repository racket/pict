#lang racket/base
(require pict
         texpict/code
         mzlib/unit
         racket/contract
         racket/class
         racket/draw
         racket/list
         racket/match
         racket/string
         syntax-color/lexer-contract
         syntax-color/racket-lexer
         "convert.rkt"
         (for-syntax racket/base
                     syntax/to-string
                     mzlib/list))

(define get-current-code-font-size (make-parameter (lambda () 12)))

(define current-code-line-sep (make-parameter 2))
(define (current-font-size) ((get-current-code-font-size)))

(define-values/invoke-unit/infer code@)

(define-code code typeset-code)

(provide code
         define-code
         (for-syntax prop:code-transformer
                     code-transformer?
                     make-code-transformer))
(provide-signature-elements
 (except code^
         typeset-code
         current-code-font
         current-code-tt
         current-comment-color
         current-keyword-color
         current-id-color
         current-literal-color
         current-const-color
         current-base-color
         current-reader-forms
         code-align
         current-keyword-list
         current-const-list
         current-literal-list))
(provide
 (contract-out
  [typeset-code (-> syntax? pict?)]
  [current-code-font (parameter/c text-style/c)]
  [current-code-tt (parameter/c (-> string? pict?))]
  [get-current-code-font-size (parameter/c (-> exact-nonnegative-integer?))]
  [current-code-line-sep (parameter/c real?)]
  [current-comment-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-keyword-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-id-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-literal-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-const-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-base-color (parameter/c (or/c string? (is-a?/c color%)))]
  [current-reader-forms (parameter/c (listof symbol?))]
  [code-align (-> pict-convertible? pict?)]
  [current-keyword-list (parameter/c (listof string?))]
  [current-const-list (parameter/c (listof string?))]
  [current-literal-list (parameter/c (listof string?))]
  [codeblock-pict (->* (string?) (#:keep-lang-line? any/c) pict?)]))

(provide define-exec-code/scale
         define-exec-code)
(define-syntax (define-exec-code/scale stx)
  (define (drop-to-run l)
    (map (lambda (x)
           (cond
            [(and (pair? (syntax-e x))
                  (eq? 'local (syntax-e (car (syntax-e x)))))
             (let ([l (syntax->list x)])
               (list* 'local
                      (drop-to-run (syntax->list (cadr l)))
                      (cddr l)))]
            [(and (pair? (syntax-e x))
                  (eq? 'define (syntax-e (car (syntax-e x)))))
             (let ([l (syntax->list x)])
               (list* 'define
                      (cadr l)
                      (drop-to-run (cddr l))))]
            [else x]))
         (filter (lambda (x)
                   (cond
                    [(eq? '_ (syntax-e x))
                     #f]
                    [(eq? '... (syntax-e x))
                     #f]
                    [(eq? 'code:blank (syntax-e x))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'code:comment (syntax-e (car (syntax-e x)))))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'code:contract (syntax-e (car (syntax-e x)))))
                     #f]
                    [(and (pair? (syntax-e x))
                          (eq? 'unsyntax (syntax-e (car (syntax-e x)))))
                     #f]
                    [else #t]))
                 l)))
  (define (drop-to-show l)
    (foldr (lambda (x r)
             (cond
              [(and (identifier? x) (eq? '_ (syntax-e x)))
               (cdr r)]
              [(and (pair? (syntax-e x))
                    (eq? 'local (syntax-e (car (syntax-e x)))))
               (cons
                (let ([l (syntax->list x)])
                  (datum->syntax
                   x
                   (list* (car l)
                          (datum->syntax
                           (cadr l)
                           (drop-to-show (syntax->list (cadr l)))
                           (cadr l))
                          (cddr l))
                   x))
                r)]
              [(and (pair? (syntax-e x))
                    (eq? 'cond (syntax-e (car (syntax-e x)))))
               (cons
                (let ([l (syntax->list x)])
                  (datum->syntax
                   x
                   (list* (car l)
                          (drop-to-show (cdr l)))
                   x))
                r)]
              [(and (pair? (syntax-e x))
                    (eq? 'define (syntax-e (car (syntax-e x)))))
               (cons (let ([l (syntax->list x)])
                       (datum->syntax
                        x
                        (list* (car l)
                               (cadr l)
                               (drop-to-show (cddr l)))
                        x))
                     r)]
              [else (cons x r)]))
           empty
           l))

  (syntax-case stx ()
    [(_ s (showable-name runnable-name string-name) . c)
     #`(begin
         (define runnable-name
           (quote-syntax
            (begin
              #,@(drop-to-run (syntax->list #'c)))))
         (define showable-name
           (scale/improve-new-text
            (code
             #,@(drop-to-show (syntax->list #'c)))
            s))
         (define string-name
           #,(syntax->string #'c)))]))

(define-syntax define-exec-code
  (syntax-rules ()
    [(_ (a b c) . r)
     (define-exec-code/scale 1 (a b c) . r)]))


;;------------------------------------------------
;; codeblock-pict

(define (tokenize/color s)
  (define lang
    (read-language (open-input-string s)
                   (lambda () (raise-argument-error
                               'codeblock-pict
                               "string containing program with #lang"
                               s))))
  (define pre-lexer
    (or (lang 'color-lexer #f)
        ;; #lang racket doesn't have a color lexer, so fall back to
        ;; the Racket lexer if we don't find one.
        racket-lexer))
  (define lexer ; based on framework/private/color
    (if (procedure-arity-includes? pre-lexer 3)
        pre-lexer ; new interface, we're good
        (lambda (in offset mode) ; old interface, need an adapter
          (let-values ([(lexeme type data new-token-start new-token-end)
                        (pre-lexer in)])
            (values lexeme type data new-token-start new-token-end 0 #f)))))
  (define port (open-input-string s)) ; reopen, to start from the beginning
  (port-count-lines! port)
  (let loop ([acc            #f]
             [rev-tokens+classes '()])
    (define-values (_1 token-class _3 start end _6 next-acc)
      (lexer port 0 acc))
    (cond
     [(equal? token-class 'eof)
      (reverse rev-tokens+classes)]
     [else
      ;; if the token has newlines, split them up, so we can recognize them
      ;; more easily later
      (define token (substring s (sub1 start) (sub1 end)))
      (define lines (add-between (string-split token "\n" #:trim? #f) "\n"))
      (define new-tokens+classes
        (for/list ([l (in-list lines)])
          (cons l token-class)))
      (loop (if (dont-stop? next-acc)
                (dont-stop-val next-acc)
                next-acc)
            (append (reverse new-tokens+classes) rev-tokens+classes))])))

(define (tokens->pict ts #:keep-lang-line? [keep-lang-line? #t])
  ;; cache parameter lookups
  (define tt (current-code-tt))
  (define id-color (current-id-color))
  (define comment-color (current-comment-color))
  (define base-color (current-base-color))
  (define literal-color (current-literal-color))
  (define keyword-color (current-keyword-color))
  (define (token-class->color c)
    (case c
      [(symbol) id-color]
      [(keyword) id-color] ; We don't have a keyword color?
      [(white-space) "white"]
      [(comment) comment-color]
      [(no-color) base-color]
      [(parenthesis) base-color] ; really? pict has no color for parens?
      [(string) literal-color]
      [(constant) literal-color]
      [(hash-colon-keyword) keyword-color]
      [else base-color])) ; 'other, or others
  (define (lang-token->pict t)
    (match-define `(,token . ,color) t)
    (hbl-append (colorize (tt "#lang") keyword-color)
                (colorize (tt (substring token 5)) id-color)))
  (define (token->pict t)
    (match-define `(,token . ,color) t)
    (colorize (tt token) (token-class->color color)))
  (define (not-newline? x) (not (equal? (car x) "\n")))
  (define lines
    (let loop ([ts ts])
      (cond
       [(empty? ts)
        '()]
       [else
        ;; take the next line
        (define-values (next-line rest)
          (splitf-at ts not-newline?))
        (cons next-line
              (loop (if (pair? rest) ; there is a newline to skip
                        (cdr rest)
                        rest)))])))
  (define first-line (car lines))
  (apply vl-append
         ;; FIXME: #lang can span lines
         ;;   (codeblock has same issue)
         (if keep-lang-line?
             (apply hbl-append (lang-token->pict (car first-line)) (map token->pict (cdr first-line)))
             (blank))
         (for/list ([line (in-list (cdr lines))])
           (apply hbl-append (map token->pict line)))))

(define (codeblock-pict s #:keep-lang-line? [keep-lang-line? #t])
  (tokens->pict (tokenize/color s) #:keep-lang-line? keep-lang-line?))
