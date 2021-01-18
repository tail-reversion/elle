#lang scribble/manual

@(require "util.rkt")


@title{Core Data Types}


@include-section["core-data/boolean.scrbl"]
@include-section["core-data/character.scrbl"]
@include-section["core-data/keyword.scrbl"]
@include-section["core-data/number.scrbl"]


@section{Optional Values}

@defidform[#:kind "type" optional]{
 An optional value encapsulates a result that either is present or absent. This data type is particularly useful for the return value of element lookups in collections, or for procedures that have a trivial notion of failure.

 @defsubform[#:kind "constructor" #:id present (present value)]{
  Container for a value.
 }

 @defsubform[#:kind "constructor" #:id absent absent]{
  Represents a lack of a value.
 }
}

@defproc[(option-case [opt option?] [#:present present-proc (λ/c any/c → any/c)] [#:absent absent-proc (λ/c → any/c)]) any/c]{

}


@section{Result Values}

@defidform[#:kind "type" result]{
 @defsubform[#:kind "constructor" #:id success (success value)]{

 }

 @defsubform[#:kind "constructor" #:id failure (failure error)]{

 }
}


@section{Symbols}

@defproc[(symbol? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a symbol; @racket[#false], otherwise.
}

@defproc[(symbol<? [s symbol?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[s]s are ordered by increasing lexicographical order; @racket[#false], otherwise.
}

@defproc[(symbol→text [s symbol?]) text?]{
 Returns text that contains the character content of @racket[s].
}

@defproc[(text→symbol [txt text?]) symbol?]{
 Returns a symbol whose name is the character content of @racket[txt].
}


@section{Text}

@defproc[(text? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a text value, @racket[#false], otherwise.
}

@defproc[(text-length [txt text?]) natural?]{
 Returns the number of characters in the @racket[txt].
}

@defproc[(text-ref [txt text?] [n natural?]) char?]{
 Returns the character located at position @racket[n] in @racket[txt], with indices begining at zero.
}

@defproc[(subtext [txt text?] [start natural?] [end natural? (text-length txt)]) text?]{
 Returns the portion of @racket[txt], begining at the index @racket[start], inclusive, and ending at the index, @racket[end], exclusive.
}

@defproc[(text-append [txt text?] ...) text?]{
 Returns a new text value that contains the concatenated character content of the @racket[txt]s, in order. In the special case of zero @racket[txt]s, returns the empty text value.
}

@defproc[(text=? [txt text?] ...+) boolean?]{
 Returns @racket[#true], if all the @racket[txt]s have identical character content; @racket[#false], otherwise.
}

@defproc[(text<? [txt text?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[txt]s are in increasing lexicographical order, given locale-insensitive, case-sensitive comparison; @racket[#false], otherwise.
}

@defproc[(text>? [txt text?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[txt]s are in decreasing lexicographical order, given locale-insensitive, case-sensitive comparison; @racket[#false], otherwise.
}

@defproc[(text≤? [txt text?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[txt]s are in nondecreasing lexicographical order, given locale-insensitive, case-sensitive comparison; @racket[#false], otherwise.
}

@defproc[(text≥? [txt text?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[txt]s are in nonincreasing lexicographical order, given locale-insensitive, case-sensitive comparison; @racket[#false], otherwise.
}
