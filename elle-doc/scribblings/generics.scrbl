#lang scribble/manual

@(require "util.rkt")


@title{Generic Interfaces}


@section{Equality}

@defproc[(=? [v1 any/c] [v2 any/c]) boolean?]{
 Returns @racket[#true], if @racket[v1] is equivalent to @racket[v2]; @racket[#false], otherwise.
}

@defproc[(≠? [v1 any/c] [v2 any/c]) boolean?]{
 Equivalent to @racket[(not (=? v1 v2))].
}


@section{Ordering}

@defproc[(ordered? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] implements the generic ordering interface; @racket[#false], otherwise.
}

@defproc[(<? [v1 ordered?] [v2 ordered?]) boolean?]{
 Returns @racket[#true], if @racket[v1] is less than @racket[v2]; @racket[#false], otherwise.
}

@defproc[(>? [v1 ordered?] [v2 ordered?]) boolean?]{
 Returns @racket[#true], if @racket[v1] is greater than @racket[v2]; @racket[#false], otherwise.
}
