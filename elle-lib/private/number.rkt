#lang elle/private/prebase

{reprovide racket/base
           #:exposing
           number?
           complex? real? rational?
           exact? inexact?
           + - * /
           zero?
           add1 sub1
           exact-integer? #:as integer?
           = #:as number=?
           < #:as number<?
           <= #:as number≤?
           > #:as number>?
           >= #:as number≥?}

{reprovide racket/math #:exposing natural? nan? infinite?}

{reprovide racket/contract
           #:exposing
           =/c #:as number=/c
           </c #:as number</c
           >/c #:as number>/c
           <=/c #:as number≤/c
           >=/c #:as number≥/c
           real-in integer-in}
