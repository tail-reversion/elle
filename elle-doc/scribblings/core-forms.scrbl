#lang scribble/manual

@(require "util.rkt")


@title{Core Syntactic Forms}

Elle differs from other Lisps in that it uses several delimiters, each having a different meaning. Elle recognizes parentheses @racket[()], brackets @racket[[]], and braces @racket[{}] as delimiters. A syntactic form may assign arbitrary meaning to the delimiters, but each has an independent meaning, when used as part of a standalone expression:

@itemlist[
 @item{parentheses are used for procedure application;}
 @item{brackets are (currently) not used for anything and will result in a syntax error;}
 @item{braces are used for “syntax application” (special forms and macros).}
 ]


@include-section["core-forms/definition.scrbl"]
@include-section["core-forms/pattern.scrbl"]
@include-section["core-forms/procedure.scrbl"]
@include-section["core-forms/local-binding.scrbl"]
@include-section["core-forms/conditional.scrbl"]
@include-section["core-forms/type.scrbl"]
