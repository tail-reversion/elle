#lang scribble/manual

@(require "util.rkt")


@title{Contracts}

Elle inherits Racket’s robust contract system.

@section{Contract Types}

@defproc[(contract? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a contract; @racket[#false], otherwise.
}

@defproc[(flat-contract? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a flat contract; @racket[#false], otherwise.
}

@defproc[(chaperone-contract? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is a chaperone contract; @racket[#false], otherwise.
}

@defproc[(impersonator-contract? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is an impersonator contract; @racket[#false], otherwise.
}


@section{Reflection}

@defproc[(contract-name [ctc contract?]) any/c]{
 Returns the name of @racket[ctc], which is used in error reporting.
}

@defproc[(has-contract? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] is contracted; @racket[#false], otherwise.
}

@defproc[(value-contract [v any/c]) (option/c contract?)]{
 Returns a @racket[present] value containing the contract of @racket[v], if it has one; @racket[absent], otherwise.
}

@defproc[(has-blame? [v any/c]) boolean?]{
 Returns @racket[#true], if @racket[v] has a contract with blame information; @racket[#false], otherwise.
}

@defproc[(value-blame [v any/c]) (option/c blame?)]{
 Returns a @racket[present] value containing the blame informmation forthe contract of @racket[v], if it has one; @racket[absent], otherwise.
}


@section{Trivial Contracts}

@defthing[any/c contract?]{
 A flat contract which matches any single value.
}

@defthing[none/c contract?]{
 A flat contract which matches no values.
}


@section{Simple Combinators}

@defproc[(∩/c [contract contract?] ...) contract?]{
 Returns a contract that is satisfied only if every @racket[contract] is satisfied.
}

@defproc[(∪/c [contract contract?] ...) contract?]{
 Returns a contract that is satisfied by any @racket[contract] given. The resulting contract checks flat contracts first.
}

@defproc[(¬/c [contract flat-contract?]) flat-contract?]{
 Returns a flat contract that requires any value that does not match @racket[contract].
}

@defproc[(if/c [predicate {λ/c any/c → boolean?}] [then-contract contract?] [else-contract contract?]) contract?]{
 Returns a contract that will first check @racket[predicate] then, if the result is @racket[#true], will check @racket[then-contract]; @racket[else-contract], otherwise.
}


@section{Procedure Contracts}

@defform/subs[#:literals (→ values any)
              {λ/c margs-spec maybe-oargs-spec maybe-rest → result-spec}
              ([margs-spec (code:line arg-ctc ... #:<kw> arg-ctc ... ...)]
               [maybe-oargs-spec (code:line)
                [arg-ctc ... #:<kw> arg-ctc ... ...]]
               [maybe-rest (code:line)
                (code:line arg-ctc #,ellipsis)]
               [result-spec result-ctc
                (values result-ctc ...)
                any])
              #:contracts ([arg-ctc contract?]
                           [result-ctc contract?])]{
 Builds a contract that requires a procedure with the specified inputs and return result. The contract will be checked each time that the procedure is called and each time it returns.
}

@defidform[any]{
 A contract that is always satisfied. Used only in the result position of procedure contracts.
}

@defidform[→]{
 Used as a literal token in some forms. Not valid as an expression.
}
