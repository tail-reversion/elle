#lang scribble/manual

@(require "util.rkt")


@title{Contracts}

Elle inherits Racket’s robust contract system.


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

@defidform[→]{
 Used as a literal token in some forms. Not valid as an expression.
}
