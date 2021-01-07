#lang scribble/manual

@(require (for-label elle))


@title[#:style '(toc)]{Elle}
@author{Kelly Smith}

Elle is an intentionally small language which shares much with Racket. Notable features include:

@itemlist[
 @item{Immutable data is emphasized.}
 @item{Pattern matching is used extensively.}
 @item{Effects, like I/O and mutation, are separated from pure functions.}
 @item{Minimal core and small standard library.}
 ]
