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
           immutable-string>=? #:as text≥?}
