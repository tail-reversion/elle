#lang scribble/manual

@(require "../util.rkt")


@title{Definitions}

Unlike most (if not all) Lisps, Elle does not have a parenthesized, prefix form for value definitions. Instead, Elle has an unparenthesized, infix form, similar to some languages outside the Lisp family.

@defform*/subs[#:link-target? #f #:id definition #:literals (= ⇒)
               [(code:line val-pat = expr)
                (code:line proc-header ⇒ expr)]
               ([proc-header (name args)
                 (proc-header args)]
                [args (code:line mandatory-args maybe-optional-args maybe-rest)]
                [mandatory-args (code:line val-pat ... #:<kw> val-pat ... ...)]
                [maybe-optional-args (code:line)
                 [val-pat ... #:<kw> val-pat ... ...]]
                [maybe-rest (code:line)
                 (code:line id #,ellipsis)])]{
 The first form binds the identifiers in the @racket[val-pat] to the corresponding values from the destructured result from the @racket[expr].

 The second form binds @racket[name] to a procedure formed by unfolding the @racket[proc-header] into a “curried @racket[λ]”.
}

@defidform[=]{
 Used as an infix token to separate binding patterns from expressions in a definition form.
}

@defidform[⇒]{
 Used as an infix token in certain forms.
}
