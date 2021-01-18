#lang scribble/manual

@(require "../util.rkt")

@title{Result Values}

@defidform[#:kind "type" result]{
 @defsubform[#:kind "constructor" #:id success (success value)]{

 }

 @defsubform[#:kind "constructor" #:id failure (failure error)]{

 }
}
