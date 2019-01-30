#lang racket/base
(require racket/contract
         racket/class
         racket/draw
         "main.rkt"
         "convert.rkt"
         "private/tidier.rkt"
         "private/layout.rkt"
         "private/hv.rkt"
         "private/naive-layered.rkt")

(provide
 (contract-out
  [rename _tree-layout
          tree-layout
          (->* () 
               (#:pict pict-convertible?) 
               #:rest (listof (or/c tree-edge? tree-layout? #f))
               tree-layout?)]
  [rename _tree-edge
          tree-edge
          (->* ((and/c _tree-layout? (not/c #f))) 
               (#:edge-color (or/c string? 
                                   (is-a?/c color%)
                                   (list/c byte? byte? byte?))
                #:edge-width (or/c 'unspecified real? #f)
                #:edge-style (or/c 'unspecified
                                   'transparent 'solid 'xor 'hilite
                                   'dot 'long-dash 'short-dash 'dot-dash
                                   'xor-dot 'xor-long-dash 'xor-short-dash
                                   'xor-dot-dash))
               tree-edge?)]
          

  [tree-edge? (-> any/c boolean?)]
  [rename _tree-layout? tree-layout? (-> any/c boolean?)]
  [binary-tree-layout? (-> any/c boolean?)]
  [binary-tidier (->* (binary-tree-layout?)
                      (#:x-spacing 
                       (or/c (and/c real? positive?) #f)
                       #:y-spacing (or/c (and/c real? positive?) #f))
                      pict?)]
  [hv-alternating (->* (binary-tree-layout?)
                       (#:x-spacing 
                        (or/c (and/c real? positive?) #f)
                        #:y-spacing (or/c (and/c real? positive?) #f))
                       pict?)]
  [naive-layered (->* (tree-layout?)
                      (#:x-spacing 
                       (or/c (and/c real? positive?) #f)
                       #:y-spacing (or/c (and/c real? positive?) #f))
                      pict?)]))

