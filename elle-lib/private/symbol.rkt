#lang elle/private/prebase

{reprovide racket/base #:exposing symbol?  symbol<?  string->symbol #:as text→symbol}
{reprovide rebellion/base/immutable-string #:exposing symbol->immutable-string #:as symbol→text}
