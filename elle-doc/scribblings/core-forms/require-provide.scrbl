#lang scribble/manual

@(require "../util.rkt")


@title{Imports and Exports}

@defform/subs[(require maybe-phase module-path imports)
              ([maybe-phase (code:line)
                (code:line #:phase phase-level)]
               [module-path root-mod-path
                (code:line root-mod-path #:submodule submod-path)]
               [root-mod-path id
                str-path]
               [submod-path [id ...+]]
               [imports (code:line #:with-prefix id)
                #:exposing-all
                (code:line #:exposing maybe-rename-id ...+)
                (code:line #:hiding id ...+)]
               [maybe-rename-id id
                (code:line id #:as id)])]{

}

@defform[(require-for-syntax module-path imports)]{
 Equivalent to @racket[(require #:phase 1 module-path imports)].
}

@defform[(require-for-template module-path imports)]{
 Equivalent to @racket[(require #:phase -1 module-path imports)].
}

@defform[(require-for-label module-path imports)]{
 Imports the specified identifiers into the @emph{label phase}.
}


@defform[(provide maybe-phase maybe-rename-id ...+)]{
 Exports the specified identifiers, with indicated renamings, at the specified phase level. If no phase level is specified, the phase defaults to @racket[0].
}

@defform[(provide-for-syntax maybe-rename-id ...+)]{
 Equivalent to @racket[(provide #:phase 1 maybe-rename-id ...)].
}

@defform[(provide-for-template maybe-rename-id ...+)]{
 Equivalent to @racket[(provide #:phase -1 maybe-rename-id ...)].
}

@defform[(provide-for-label maybe-rename-id ...+)]{
 Exports the specified identifers, with indicated renamings, at the @emph{label phase}.
}


@section{Reexports}

@defform/subs[(reprovide maybe-phase module-path rexports)
              ([reexports #:exposing-all
                (code:line #:exposing maybe-rename-id ...+)
                (code:line #:hiding id ...+)])]{
 Reexports the specified identifies, with indicated renamings.

 The reexported identifiers are @emph{not} available within the body of the module.
}
