#lang elle/private/prebase

{reprovide racket/base #:exposing symbol?  symbol<?  string->symbol #:as text→symbol}
{reprovide racket/symbol #:exposing symbol->immutable-string #:as symbol→text}
