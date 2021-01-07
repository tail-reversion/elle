#lang elle/private/prebase

{provide boolean?
         not negate
         and nand
         or nor
         xor nxor
         implies}

{require racket/base #:exposing boolean?  not #:as rkt:not  and #:as rkt:and  or #:as rkt:or  foldl #:as list-foldl}
{require racket/bool #:exposing xor #:as rkt:xor  implies #:as rkt:implies}
{require elle/private/procedure #:exposing procedure? ∘}


not : {λ/c boolean? → boolean?}
not = rkt:not

negate : {λ/c procedure? → procedure?}
(negate pred) ⇒ (∘ not pred)

and : {λ/c boolean? ... → boolean?}
(and bs ...) ⇒ (list-foldl {λ x y ⇒ {rkt:and x y}} #true bs)

nand : {λ/c boolean? ... → boolean?}
nand = (∘ not and)

or : {λ/c boolean? ... → boolean?}
(or bs ...) ⇒ (list-foldl {λ x y ⇒ {rkt:or x y}} #false bs)

nor : {λ/c boolean? ... → boolean?}
nor = (∘ not or)

xor : {λ/c boolean? ... → boolean?}
(xor bs ...) ⇒ (list-foldl {λ x y ⇒ (rkt:xor x y)} #false bs)

nxor : {λ/c boolean? ... → boolean?}
nxor = (∘ not xor)

implies : {λ/c boolean? boolean? → boolean?}
(implies b1 b2) ⇒ {rkt:implies b1 b2}
