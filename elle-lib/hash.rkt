#lang elle/base

{provide @%hash #:as hash
         hash-set
         hash-remove
         hash-has-key?
         hash-count
         @%hash/c #:as hash/c}

{reprovide rebellion/collection/hash
           #:exposing
           immutable-hash? #:as hash?
           empty-immutable-hash? #:as empty-hash?
           hash-ref-safe #:as hash-ref
           hash-set-entry}

{require-for-syntax racket/base #:exposing-all}
{require-for-syntax syntax/parse #:exposing-all}

{require racket/base #:exposing hash hash-ref hash-set hash-set* hash-has-key? hash-remove hash-count}
{require racket/contract #:exposing hash/c}
{require racket/match #:exposing define-match-expander}


@%hash/c : {λ/c chaperone-contract? contract? [#:flat boolean?] → contract?}
(@%hash/c key val [#:flat? flat? = #false]) ⇒ (hash/c key val #:immutable #true #:flat? flat?)

#(define-match-expander @%hash
   (syntax-parser
     #:track-literals
     #:literals (:)
     [(_ {~seq key : val} ...) #'(hash-table (key val) ...)])
   (syntax-parser
     #:track-literals
     #:literals (:)
     [(_ {~seq key : val} ...) #'(hash {~@ key val} ...)]))
