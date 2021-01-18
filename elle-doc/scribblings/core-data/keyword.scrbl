#lang scribble/manual

@(require "../util.rkt")


@title{Keywords}

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
