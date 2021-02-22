#lang scribble/manual

@(require "../util.rkt")


@title{Definitions}

Unlike most (if not all) Lisps, Elle does not have a parenthesized, prefix form for value definitions. Instead, Elle has an unparenthesized, infix form, similar to some languages outside the Lisp family.

@defform*/subs[#:link-target? #f #:id definition #:literals (= ⇒)
               [(code:line id = expr)
                (code:line proc-header ⇒ expr)]
               ([proc-header (name args)
                 (proc-header args)]
                [args (code:line mandatory-args maybe-optional-args maybe-rest)]
                [mandatory-args (code:line id ... #:<kw> id ... ...)]
                [maybe-optional-args (code:line)
                 [id ... #:<kw> id ... ...]]
                [maybe-rest (code:line)
                 (code:line id #,ellipsis)])]{
 The first form binds @racket[id] to the result of @racket[expr].

 The second form binds @racket[name] to a procedure formed by unfolding the @racket[proc-header] into a “curried @racket[λ]”.
}

@defidform[=]{
 Used as an infix token to separate binding patterns from expressions in a definition form.
}

@defidform[⇒]{
 Used as an infix token in certain forms.
}
