#lang elle/private/prebase

{provide @%list #:as list}

{reprovide racket/base
           #:exposing
           list?
           map #:as list-map
           filter #:as list-filter
           foldl #:as list-foldl
           foldr #:as list-foldr}

{reprovide racket/list
           #:exposing
           take #:as list-take
           take-right #:as list-take-right
           drop #:as list-drop
           drop-right #:as list-drop-right
           split-at #:as list-split-at
           split-at-right #:as list-split-at-right
           filter-map #:as list-filter-map
           filter-not #:as list-filter-not
           make-list
           list-update
           list-set}

{reprovide rebellion/collection/list
           #:exposing
           empty-list empty-list? nonempty-list?
           list-first  list-rest  list-ref-safe #:as list-ref  list-size
           list-insert list-append
           list-reverse}


{require-for-syntax racket/base #:hiding list}
{require-for-syntax syntax/parse #:exposing-all}

{require racket/base #:exposing list? list list*}
{require racket/match #:exposing define-match-expander}


#(define-match-expander @%list
   (syntax-parser
     #:track-literals
     [(_ elt ...) #'(list elt ...)])
   (syntax-parser
     #:track-literals
     [(_ elt:expr ... lst-elt {~literal ...} other:expr ...+)
      #:declare lst-elt (expr/c #'list?)
      #'(list* elt ... (list-append lst-elt.c (@%list other ...)))]
     [(_ elt:expr ... lst-elt {~literal ...})
      #:declare lst-elt (expr/c #'list?)
      #'(list* elt ... lst-elt.c)]
     [(_ elt:expr ...) #'(list elt ...)]))
