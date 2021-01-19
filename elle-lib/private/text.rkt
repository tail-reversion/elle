#lang elle/private/prebase

{reprovide rebellion/base/immutable-string
           #:exposing
           immutable-string? #:as text?
           immutable-string-length #:as text-length
           immutable-string-ref #:as text-ref
           immutable-substring #:as subtext
           immutable-string-append #:as text-append
           immutable-string=? #:as text=?
           immutable-string<? #:as text<?
           immutable-string<=? #:as text≤?
           immutable-string>? #:as text>?
           immutable-string>=? #:as text≥?
           immutable-string-ci=? #:as text-ci=?
           immutable-string-ci<? #:as text-ci<?
           immutable-string-ci>? #:as text-ci>?
           immutable-string-ci<=? #:as text-ci≤?
           immutable-string-ci>=? #:as text-ci≥?
           immutable-string-upcase #:as text-uppercase
           immutable-string-downcase #:as text-lowercase
           immutable-string-titlecase #:as text-titlecase
           immutable-string-foldcase #:as text-foldcase
           immutable-string-normalize-nfc #:as text-normalize-nfc
           immutable-string-normalize-nfd #:as text-normalize-nfd
           immutable-string-normalize-nfkc #:as text-normalize-nfkc
           immutable-string-normalize-nfkd #:as text-normalize-nfkd}
