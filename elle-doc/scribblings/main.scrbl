#lang scribble/manual

@(require "util.rkt")


@title[#:style '(toc)]{Elle}
@author{Kelly Smith}

@defmodule[elle #:lang]

@nested[#:style 'inset]{
 @bold{WARNING:} The Elle language is currently under development. The language itself is @emph{unstable,} and this manual @emph{may not} accurately reflect the current language syntax, features, and libraries.

  You have been warned.
}

Elle is an intentionally small language which shares much with Racket. Notable features include:

@itemlist[
 @item{Immutable data is emphasized.}
 @item{Pattern matching is used extensively.}
 @item{Effects, like I/O and mutation, are separated from pure functions.}
 @item{Minimal core and small standard library.}
 ]


@table-of-contents[]

@include-section["core-forms.scrbl"]
@include-section["core-data.scrbl"]
