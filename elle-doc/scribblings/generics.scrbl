#lang scribble/manual

@(require "util.rkt")


@title{Generic Interfaces}


@section{Equality}

@defproc[(=? [v1 any/c] [v2 any/c]) boolean?]{

}

@defproc[(≠? [v1 any/c] [v2 any/c]) boolean?]{
Equivalent to @racket[(not (=? v1 v2))].
}


@section{Ordering}

@defproc[(ordered? [v any/c]) boolean?]{

}

@defproc[(<? [v1 ordered?] [v2 ordered?]) boolean?]{

}

@defproc[(>? [v1 ordered?] [v2 ordered?]) boolean?]{

}
