#lang elle/base

{provide hash-set
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

{require racket/base #:exposing hash hash-ref hash-set hash-set* hash-has-key? hash-remove hash-count}
{require racket/contract #:exposing hash/c}


@%hash/c : {λ/c chaperone-contract? contract? [#:flat boolean?] → contract?}
(@%hash/c key val [#:flat? flat? = #false]) ⇒ (hash/c key val #:immutable #true #:flat? flat?)
