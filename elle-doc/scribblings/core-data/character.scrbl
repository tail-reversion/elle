#lang scribble/manual

@(require "../util.rkt")


@title{Characters}

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


@section{Comparison}

@defproc[(char=? [c char?] ...+) boolean?]{
 Returns @racket[#true], if the @racket[c]s are equivalent; @racket[#false], otherwise.
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


@subsection{Case-Insensitive Comparison}

@defproc[(char-ci=? [c char?] ...+) boolean?]{
 Like @racket[char=?], but case-insensitive.
}

@defproc[(char-ci<? [c char?] ...+) boolean?]{
 Like @racket[char<?], but case-insensitive.
}

@defproc[(char-ci>? [c char?] ...+) boolean?]{
 Like @racket[char>?], but case-insensitive.
}

@defproc[(char-ci≤? [c char?] ...+) boolean?]{
 Like @racket[char≤?], but case-insensitive.
}

@defproc[(char-ci≥? [c char?] ...+) boolean?]{
 Like @racket[char≥?], but case-insensitive.
}


@section{Contracts}

@defproc[(char-in [c1 char?] [c2 char?]) flat-contract?]{
 Returns a flat contract that recognizes a character with a code point between that of @racket[c1] and @racket[c2], inclusive.
}


@section{Conversions}

@defproc[(char-uppercase [c char?]) char?]{
 Returns the uppercase character associated with @racket[c], as defined by Unicode. In the case that no such mapping is defined, returns @racket[c].
}

@defproc[(char-lowercase [c char?]) char?]{
 Like @racket[char-uppercase], but for lowercase mapping.
}

@defproc[(char-titlecase [c char?]) char?]{
 Like @racket[char-uppercase], but for titlecase mapping.
}

@defproc[(char-foldcase [c char?]) char?]{
 Like @racket[char-uppercase], but for case-folding mapping.
}
