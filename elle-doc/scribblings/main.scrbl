#lang scribble/manual

@(require (for-label elle))


@title[#:style '(toc)]{Elle}
@author{Kelly Smith}

@nested{
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
