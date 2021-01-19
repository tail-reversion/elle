#lang scribble/manual

@(require "../util.rkt")


@title{Local Binding}

@defform[{let definition ...+ #:in expr}]{
 Performs the given definitions and makes available their bindings in the body of @racket[expr], which is evaluated in tail position, with respect to the @racket[let] form.
}
