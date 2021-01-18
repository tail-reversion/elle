#lang scribble/manual

@(require "../util.rkt")


@title{Symbols}

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
