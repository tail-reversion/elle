#lang elle/private/prebase

{reprovide rebellion/binary/immutable-bytes
           #:exposing
           immutable-bytes? #:as bytes?
           immutable-bytes=? #:as bytes=?
           immutable-bytes<? #:as bytes<?
           immutable-bytes>? #:as bytes>?
           immutable-bytes->string/utf-8 #:as bytes→text
           immutable-string->bytes/utf-8 #:as text→bytes/utf-8}
