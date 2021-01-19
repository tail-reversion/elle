#lang scribble/manual

@(require "../util.rkt")


@title{Conditional Forms}

@defform[{if test-expr then-expr else-expr}
         #:contracts ([test-expr boolean?])]{
 Evaluates @racket[test-expr]; if the result is @racket[#true], the @racket[then-expr] is evaluated; if the result is @racket[#false], the @racket[else-expr] is evaluated. Note that, unlike Scheme and Racket, Elle’s @racket[if] requires that the test expression be a boolean value.
}

@defform/subs[#:literals (⇒)
              {cond cond-clause ...+ maybe-else}
              ([cond-clause (code:line test-expr ⇒ then-expr)]
               [maybe-else (code:line)
                (code:line #:else else-expr)])
              #:contracts ([test-expr boolean?])]{
 Evaluates each @racket[test-expr], in order; at the first to return @racket[#true], the associated @racket[then-expr] is evaluated in tail position, with respect to the @racket[cond] form. If no @racket[test-expr] is @racket[#true], then the @racket[else-expr], if present, is evaluated in tail position, with respect to the @racket[cond] form. If no @racket[test-expr] is @racket[#true], and there is no @racket[else-expr], an exception will be thrown.
}


@defform[{when test-expr result-expr}
         #:contracts ([test-expr boolean?])]{
 Evaluates @racket[test-expr], and if the value is @racket[#true], then @racket[result-expr] is evaluated in tail position, with respect to the @racket[when] form. If @racket[test-expr] is @racket[#false], the result of the @racket[when] form is an zero-value return.
}

@defform[{unless test-expr result-expr}
         #:contracts ([test-expr boolean?])]{
 Equivalent to @racket[{when (not test-expr) result-expr}].
}
