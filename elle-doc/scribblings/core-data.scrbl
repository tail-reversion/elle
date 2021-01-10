#lang scribble/manual

@(require "util.rkt")


@title{Core Data Types}


@section{Booleans}

@defproc[(boolean? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a Boolean value; @racket[#false], otherwise.
}

@defproc[(not [b boolean?]) boolean?]{

}

@; TODO: negate

@defproc[(and [b boolean?] ...) boolean?]{

}

@defproc[(nand [b boolean?] ...) boolean?]{
 Equivalent to @racket[(not (and b ...))].
}

@defproc[(or [b boolean?] ...) boolean?]{

}

@defproc[(nor [b boolean?] ...) boolean?]{
 Equivalent to @racket[(not (or b ...))].
}

@defproc[(xor [b boolean?] ...) boolean?]{

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

}

@defproc[(unicode-scalar-value→char [n unicode-scalar-value?]) char?]{

}

@defproc[(char<? [c1 char?] [c2 char?]) boolean?]{

}

@defproc[(char>? [c1 char?] [c2 char?]) boolean?]{

}

@defproc[(char≤? [c1 char?] [c2 char?]) boolean?]{

}

@defproc[(char≥? [c1 char?] [c2 char?]) boolean?]{

}

@defproc[(char-in [c1 char?] [c2 char?]) flat-contract?]{

}


@section{Keywords}

@defproc[(keyword? [v any/c]) boolean?]{

}

@defproc[(keyword<? [kw1 keyword?] [kw2 keyword?]) boolean?]{

}

@defproc[(keyword→text [kw keyword?]) text?]{

}

@defproc[(text→keyword [txt text?]) keyword?]{

}


@section{Numerical Types and Operations}

@subsection{Types}

@defproc[(number? [v any/c]) boolean?]{

}

@defproc[(complex? [v any/c]) boolean?]{

}

@defproc[(real? [v any/c]) boolean?]{

}

@defproc[(rational? [v any/c]) boolean?]{

}

@defproc[(integer? [v any/c]) boolean?]{

}

@defproc[(natural? [v any/c]) boolean?]{

}

@defproc[(exact? [z number?]) boolean?]{

}

@defproc[(inexact? [z number?]) boolean?]{

}


@subsection{Generic Arithmethic}

@defproc[(+ [z number?] ...) number?]{

}

@defproc[(- [z number?] ...+) number?]{

}

@defproc[(* [z number?] ...) number?]{

}

@defproc[(/ [z number?] ...+) number?]{

}

@defproc[(add1 [z number?]) number?]{

}

@defproc[(sub1 [z number?]) number?]{

}

@defproc[(zero? [z number?]) boolean?]{

}


@subsection{Comparison}

@defproc[(number=? [z1 number?] [z number?] ...) boolean?]{

}

@defproc[(number<? [x1 real?] [x real?] ...) boolean?]{

}

@defproc[(number>? [x1 real?] [x real?] ...) boolean?]{

}

@defproc[(number≤? [x1 real?] [x real?] ...) boolean?]{

}

@defproc[(number≥? [x1 real?] [x real?] ...) boolean?]{

}


@subsection{Contracts}

@defproc[(number=/c [x real?]) flat-contract?]{

}

@defproc[(number</c [x real?]) flat-contract?]{

}

@defproc[(number>/c [x real?]) flat-contract?]{

}

@defproc[(number≤/c [x real?]) flat-contract?]{

}

@defproc[(number≥/c [x real?]) flat-contract?]{

}

@defproc[(real-in [x1 real?] [x2 real?]) flat-contract?]{

}

@defproc[(integer-in [i1 integer?] [i2 integer?]) flat-contract?]{

}


@section{Optional Values}

@defproc[(optional? [v any/c]) boolean?]{

}

@defproc[(absent? [v any/c]) boolean?]{

}

@defthing[absent absent?]{

}

@defproc[(present? [v any/c]) boolean?]{

}

@defproc[(present [v any/c]) present?]{

}

@defproc[(present-value [p present?]) any/c]{

}

@defproc[(option-case [o option?] [#:present present-proc (λ/c any/c → any/c)] [#:absent absent-proc (λ/c → any/c)]) any/c]{

}
