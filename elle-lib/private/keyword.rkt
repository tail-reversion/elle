#lang elle/private/prebase

{reprovide racket/base #:exposing keyword?  keyword<?  string->keyword #:as text→keyword}
{reprovide racket/keyword #:exposing keyword->immutable-string #:as keyword→text}
