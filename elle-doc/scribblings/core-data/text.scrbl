#lang scribble/manual

@(require "../util.rkt")


@title{Text}
Elle’s text data type is an immutable string of Unicode characters (code points).

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


@section{Comparison}

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


@subsection{Case-Insensitive Comparison}

@defproc[(text-ci=? [txt text?] ...+) boolean?]{
 Like @racket[text=?], but case-insensitive.
}

@defproc[(text-ci<? [txt text?] ...+) boolean?]{
 Like @racket[text<?], but case-insensitive.
}

@defproc[(text-ci>? [txt text?] ...+) boolean?]{
 Like @racket[text>?], but case-insenstive.
}

@defproc[(text-ci≤? [txt text?] ...+) boolean?]{
 Like @racket[text≤?], but case-insensitive.
}

@defproc[(text-ci≥? [txt text?] ...+) boolean?]{
 Like @racket[text≥?], but case-insensitive.
}


@subsection{Case Mapping}

@defproc[(text-uppercase [txt text?]) text?]{
 Returns text whose character content has been mapped to uppercase characters, given Unicode’s local-independent uppercase mapping rules.
}

@defproc[(text-lowercase [txt text?]) text?]{
 Like @racket[text-uppercase], but for lowercase mapping.
}

@defproc[(text-titlecase [txt text?]) text?]{
 Like @racket[text-uppercase], but for titlecase mapping.
}

@defproc[(text-foldcase [txt text?]) text?]{
 Like @racket[text-uppercase], but for case-folding.
}


@subsection{Normalization}

@defproc[(text-normalize-nfd [txt text?]) text?]{
 Returns text that is normalized according to Unicode Normalized Form D.
}

@defproc[(text-normalize-nfc [txt text?]) text?]{
 Like @racket[text-normalize-nfd], but for Normalized Form C.
}

@defproc[(text-normalize-nfkd [txt text?]) text?]{
 Like @racket[text-normalize-nfd], but for Normalized Form KD.
}

@defproc[(text-normalize-nfkc [txt text?]) text?]{
 Like @racket[text-normalize-nfd], but for Normalized Form KC.
}
