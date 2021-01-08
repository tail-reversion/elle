#lang scribble/manual

@(require "util.rkt")


@title{Core Syntactic Forms}


@section{Definitions}

Unlike most (if not all) Lisps, Elle does not have a parenthesized, prefix form for value definitions. Instead, Elle has an unparenthesized, infix form, similar to some languages outside the Lisp family.

@defform*/subs[#:link-target? #f #:id definition #:literals (= ⇒)
               [(code:line val-pat = expr)
                (code:line proc-header ⇒ expr)]
               ([proc-header (name args)
                 (proc-header args)]
                [args (code:line mandatory-args maybe-optional-args maybe-rest)]
                [mandatory-args (code:line val-pat ... #:⟨kw⟩ val-pat ... ...)]
                [maybe-optional-args (code:line)
                 [val-pat ... #:⟨kw⟩ val-pat ... ...]]
                [maybe-rest (code:line)
                 (code:line id #,ellipsis)])]{
 The first form binds the identifiers in the @racket[val-pat] to the corresponding values from the destructured result from the @racket[expr].

 The second form binds @racket[name] to a procedure formed by unfolding the @racket[proc-header] into a “curried @racket[λ]”.
}


@defidform[=]{
 Used as a token to separate binding patterns from expressions in a definition form.
}

@defidform[⇒]{
 Used as a token to separate procedure headers from procedure bodies in a definition form.
}


@section{Patterns}

@defform*[#:link-target? #f #:id match-pat
          [lit
           {derived stx ...}]]{

}


@defform[#:link-target? #f #:id vals-pat #:literals (values)
         (values pat ...)]{

}


@section{Procedures}

@defform*[#:literals (⇒)
          ({λ args ⇒ expr}
           {λ args ⇒ λ args ⇒ ... ... ... expr})]{
 The first form creates an anonymous procedure that is closed over its lexical environment.

 The second form is syntaxtic sugar for nested @racket[λ] forms, yielding a, “curried @racket[λ]”.
}


@section{Local Binding}

@defform[{let definition ...+ #:in expr}]{
 Performs the given definitions and makes available their bindings in the body of @racket[expr].
}
