#lang scribble/manual

@(require "../util.rkt")


@title{Booleans}

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
