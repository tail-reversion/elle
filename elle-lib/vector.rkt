#lang elle/base

{provide @%vector #:as vector
         immutable-vector? #:as vector?
         make-immutable-vector #:as make-vector
         immutable-vector-length #:as vector-length
         immutable-vector-ref #:as vector-ref
         immutable-vector->list #:as vector→list
         list->immutable-vector #:as list→vector
         immutable-vector->values #:as vector→values
         build-immutable-vector  #:as build-vector
         immutable-vector-map #:as vector-map
         immutable-vector-append #:as vector-append
         immutable-vector-take #:as vector-take
         immutable-vector-take-right #:as vector-take-right
         immutable-vector-drop #:as vector-drop
         immutable-vector-drop-right #:as vector-drop-right
         immutable-vector-split-at #:as vector-split-at
         immutable-vector-split-at-right #:as vector-split-at-right
         immutable-vector-copy #:as vector-copy
         immutable-vector-filter #:as vector-filter
         immutable-vector-filter-not #:as vector-filter-not
         empty-immutable-vector #:as empty-vector
         empty-immutable-vector? #:as empty-vector?
         nonempty-immutable-vector? #:as nonempty-vector?}

{require rebellion/collection/immutable-vector #:exposing-all}
{require racket/base #:exposing vector}
{require racket/match #:exposing define-match-expander}

{require-for-syntax racket/base #:exposing-all}
{require-for-syntax syntax/parse #:exposing-all}


#(define-match-expander @%vector
   (syntax-parser
     #:track-literals
     [(_ elt ...) #'(vector elt ...)])
   (syntax-parser
     #:track-literals
     [(_ elt:expr ...) #'(immutable-vector elt ...)]))
