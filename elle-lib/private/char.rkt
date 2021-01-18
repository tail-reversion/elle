#lang elle/private/prebase

{provide unicode-scalar-value?
         char→unicode-scalar-value
         unicode-scalar-value→char}

{reprovide racket/base
           #:exposing
           char?
           char=?
           char<?
           char>?
           char<=? #:as char≤?
           char>=? #:as char≥?
           char-ci=?
           char-ci<?
           char-ci>?
           char-ci<=? #:as char-ci≤?
           char-ci>=? #:as char-ci≥?}

{reprovide racket/contract #:exposing char-in}

{require racket/base #:exposing char? char->integer integer->char}
{require elle/private/number #:exposing-all}


unicode-scalar-value? = (flat-named-contract 'unicode-scalar-value?
                                             (∪/c (integer-in #x0 #xd7ff)
                                                  (integer-in #xe000 #x10ffff)))

char→unicode-scalar-value : {λ/c char? → unicode-scalar-value?}
char→unicode-scalar-value = char->integer

unicode-scalar-value→char : {λ/c unicode-scalar-value? → char?}
unicode-scalar-value→char = integer->char
