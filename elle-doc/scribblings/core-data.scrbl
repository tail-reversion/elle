#lang scribble/manual

@(require "util.rkt")


@title{Core Data Types}


@section{Booleans}

@defproc[(boolean? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a Boolean value; @racket[#false], otherwise.
}

@defproc[(not [b boolean?]) boolean?]{
 Returns the logical complement of @racket[b].
}

@; TODO: negate

@defproc[(and [b boolean?] ...) boolean?]{
 Returns the logical conjunction of the @racket[b]s. In the special case of zero arguments, returns @racket[#true].
}

@defproc[(nand [b boolean?] ...) boolean?]{
 Equivalent to @racket[(not (and b ...))].
}

@defproc[(or [b boolean?] ...) boolean?]{
 Returns the logical inclusive disjunction of the @racket[b]s. In the special case of zero arguments, returns @racket[#false].
}

@defproc[(nor [b boolean?] ...) boolean?]{
 Equivalent to @racket[(not (or b ...))].
}

@defproc[(xor [b boolean?] ...) boolean?]{
 Returns the logical exclusive disjunction of the @racket[b]s. In the special case of zero arguments, returns @racket[#false].
}

@defproc[(nxor [b boolean?] ...) boolean?]{
 Equivalent to @racket[(not (xor b ...))].
}

@; TODO: implies


@section{Characters}

@defproc[(char? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a character; @racket[#false], otherwise.
}

@defproc[(unicode-scalar-value? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a Unicode Scalar Value; @racket[#false], otherwise.
}

@defproc[(char→unicode-scalar-value [c char?]) unicode-scalar-value?]{
 Returns a Unicode Scalar Value that represents @racket[c].
}

@defproc[(unicode-scalar-value→char [n unicode-scalar-value?]) char?]{
 Returns the character represented by @racket[n].
}

@defproc[(char<? [c char?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[c]s are ordered by increasing Scalar Values; @racket[#false], otherwise.
}

@defproc[(char>? [c char?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[c]s are ordered by decreasing Scalar Values; @racket[#false], otherwise.
}

@defproc[(char≤? [c char?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[c]s are ordered by nondecreasing Scalar Values; @racket[#false], otherwise.
}

@defproc[(char≥? [c char?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[c]s are ordered by nonincreasing Scalar Values; @racket[#false], otherwise.
}

@defproc[(char-in [c1 char?] [c2 char?]) flat-contract?]{
 Returns a flat contract that recognizes a character with a code point between that of @racket[c1] and @racket[c2], inclusive.
}


@section{Keywords}

@defproc[(keyword? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a keyword; @racket[#false], otherwise.
}

@defproc[(keyword<? [kw1 keyword?] [kw2 keyword?]) boolean?]{
 Returns @racket[#true], if @racket[kw1] and @racket[kw2] are in lexicographically-increasing order; @racket[#false], otherwise.
}

@defproc[(keyword→text [kw keyword?]) text?]{
 Returns a text value whose character content is the keyword name.
}

@defproc[(text→keyword [txt text?]) keyword?]{
 Returns a keyword whose name is the character content of @racket[txt].
}


@section{Numerical Types and Operations}
Elle inherits the Scheme Numerical Tower. More precisely, Elle inherits Racket’s implementation of the Numerical Tower.

@subsection{Types}

@defproc[(number? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a number; @racket[#false], otherwise. This procedure is an alias for @racket[complex?], since all numbers are complex numbers.
}

@defproc[(complex? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a complex number; @racket[#false], otherwise.
}

@defproc[(real? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a complex number whose imaginary part is zero.
}

@defproc[(rational? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a rational number; @racket[#false], otherwise.
}

@defproc[(integer? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is an integer; @racket[#false], otherwise.
}

@defproc[(natural? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a natural number; @racket[#false], otherwise.
}

@defproc[(exact? [z number?]) boolean?]{
 Returns @racket[#true], if @racket[z] is an exact number; @racket[#false], otherwise.
}

@defproc[(inexact? [z number?]) boolean?]{
 Returns @racket[#true], if @racket[z] is an inexact number; @racket[#false], otherwise.
}


@subsection{Generic Arithmethic}

@defproc[(+ [z number?] ...) number?]{
 Returns the sum of the @racket[z]s. In the special case of one @racket[z], this procedure is the identity. In the special case of zero @racket[z]s, returns an exact zero.
}

@defproc[(- [z number?] ...+) number?]{
 Returns the differents of the @racket[z]s. In the special case of one @racket[z], returns the additive inverse of the @racket[z].
}

@defproc[(* [z number?] ...) number?]{
 Returns the product of the @racket[z]s. In the special case of one @racket[z], this procedure is the identity. In the special case of zero @racket[z]s, returns an exact one.
}

@defproc[(/ [z number?] ...+) number?]{
 Returns the quotient of the @racket[z]s. In the special case of one @racket[z], returns the multiplicative inverse (reciprocal) of @racket[z].
}

@defproc[(add1 [z number?]) number?]{
 Equivalent to @racket[(+ z 1)].
}

@defproc[(sub1 [z number?]) number?]{
 Equivalent to @racket[(- z 1)].
}

@defproc[(zero? [z number?]) boolean?]{
 Returns @racket[#true], if @racket[z] is exact or inexact zero; @racket[#false], otherwise.
}


@subsection{Comparison}

@defproc[(number=? [z number?] ...+) boolean?]{
 Returns @racket[#true], if all @racket[z]s are numerically equivalent; @racket[#false], otherwise. Inexact numbers are coerced to exactness before comparison.
}

@defproc[(number<? [x real?] ...+) boolean?]{
 Returns @racket[#true], if all @racket[x]s are ordered by numerically increasing value; @racket[#false], otherwise.
}

@defproc[(number>? [x real?] ...+) boolean?]{
 Returns @racket[#true], if all @racket[x]s are ordered by numerically decreasing value; @racket[#false], otherwise.
}

@defproc[(number≤? [x real?] ...+) boolean?]{
 Returns @racket[#true], if all @racket[x]s are ordered by numerically nondecreasing value; @racket[#false], otherwise.
}

@defproc[(number≥? [x real?] ...+) boolean?]{
 Returns @racket[#true], if all @racket[x]s are ordered by numerically nonincreasing value; @racket[#false], otherwise.
}


@subsection{Contracts}

@defproc[(number=/c [x real?]) flat-contract?]{
 Returns a flat contract that requires a real number that is numerically equivalent to @racket[x].
}

@defproc[(number</c [x real?]) flat-contract?]{
 Returns a flat contract that requires a real number that numerically less than @racket[x].
}

@defproc[(number>/c [x real?]) flat-contract?]{
 Returns a flat contract that requires a real number that is numerically greater than @racket[x].
}

@defproc[(number≤/c [x real?]) flat-contract?]{
 Returns a flat contract that requires a real nummber that is either numerically equivalent or less than @racket[x].
}

@defproc[(number≥/c [x real?]) flat-contract?]{
 Returns a flat contract that requires a real number that is either numerically equivalent or greater than @racket[x].
}

@defproc[(real-in [x1 real?] [x2 real?]) flat-contract?]{
 Returns a flat contract that requires a real number that is between @racket[x1] and @racket[x2], inclusive.
}

@defproc[(integer-in [i1 integer?] [i2 integer?]) flat-contract?]{
 Returns a flat contract that requires an integer that is between @racket[i1] and @racket[i2], inclusive.
}


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
