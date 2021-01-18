#lang scribble/manual

@(require "../util.rkt")


@title{Optional Values}

@defidform[#:kind "type" optional]{
 An optional value encapsulates a result that either is present or absent. This data type is particularly useful for the return value of element lookups in collections, or for procedures that have a trivial notion of failure.

 @defsubform[#:kind "constructor" #:id present (present value)]{
  Container for a value.
 }

 @defsubform[#:kind "constructor" #:id absent absent]{
  Represents a lack of a value.
 }
}

@defproc[(option-case [opt option?] [#:present present-proc (λ/c any/c → any/c)] [#:absent absent-proc (λ/c → any/c)]) any/c]{

}
