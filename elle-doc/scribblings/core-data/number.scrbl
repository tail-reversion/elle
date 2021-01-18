#lang scribble/manual

@(require "../util.rkt")


@title{Numbers}
Elle inherits the Scheme Numerical Tower. More precisely, Elle inherits Racket’s implementation of the Numerical Tower.

@section{Types}

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


@section{Generic Arithmethic}

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
