#lang scribble/manual

@(require "util.rkt")


@title{Parameters}

@defproc[(parameter? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a parameter; @racket[#false], otherwise.
}

@racket[(make-parameter [initial any/c] [#:name name symbol? 'parameter-procedure] [#:guard guard (option/c {λ/c any/c → #:unconstrained})]) parameter?]{
 Returns a new parameter, with initial value @racket[initial], name @racket[name], and guard @racket[guard].
}

@defform/subs[#:literals (←)
              (parameterize set-param ...+ #:in body)
              ([set-param (code:line param ← expr)])
              #:contracts ([param parameter?])]{
 Assigns each @racket[param] the given value, then evaluates the @racket[body] expression in tail position, with respect to the @racket[parameterize] form.
}

@defform[#:literals (←)
         (parameterize* set-param ...+ #:in body)]{
 Equivalent to a nested sequence of @racket[parameterize] forms.
}
