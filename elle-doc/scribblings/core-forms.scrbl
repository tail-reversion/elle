#lang scribble/manual

@(require "util.rkt")


@title{Core Syntactic Forms}

Elle differs from other Lisps in that it uses several delimiters, each having a different meaning. Elle recognizes parentheses @racket[()], brackets @racket[[]], and braces @racket[{}] as delimiters. A syntactic form may assign arbitrary meaning to the delimiters, but each has an independent meaning, when used as part of a standalone expression:

@itemlist[
 @item{parentheses are used for procedure application;}
 @item{brackets are (currently) not used for anything and will result in a syntax error;}
 @item{braces are used for “syntax application” (special forms and macros).}
 ]


@include-section["core-forms/definition.scrbl"]
@include-section["core-forms/pattern.scrbl"]
@include-section["core-forms/procedure.scrbl"]
@include-section["core-forms/local-binding.scrbl"]


@section{Conditional Forms}

@defform[{if test-expr then-expr else-expr}
         #:contracts ([test-expr boolean?])]{
 Evaluates @racket[test-expr]; if the result is @racket[#true], the @racket[then-expr] is evaluated; if the result is @racket[#false], the @racket[else-expr] is evaluated. Note that, unlike Scheme and Racket, Elle’s @racket[if] requires that the test expression be a boolean value.
}

@defform/subs[#:literals (⇒)
              {cond cond-clause ...+ maybe-else}
              ([cond-clause (code:line test-expr ⇒ then-expr)]
               [maybe-else (code:line)
                (code:line #:else else-expr)])
              #:contracts ([test-expr boolean?])]{
 Evaluates each @racket[test-expr], in order; at the first to return @racket[#true], the associated @racket[then-expr] is evaluated in tail position, with respect to the @racket[cond] form. If no @racket[test-expr] is @racket[#true], then the @racket[else-expr], if present, is evaluated in tail position, with respect to the @racket[cond] form. If no @racket[test-expr] is @racket[#true], and there is no @racket[else-expr], an exception will be thrown.
}


@defform[{when test-expr result-expr}
         #:contracts ([test-expr boolean?])]{
 Evaluates @racket[test-expr], and if the value is @racket[#true], then @racket[result-expr] is evaluated in tail position, with respect to the @racket[when] form. If @racket[test-expr] is @racket[#false], the result of the @racket[when] form is an zero-value return.
}

@defform[{unless test-expr result-expr}
         #:contracts ([test-expr boolean?])]{
 Equivalent to @racket[{when (not test-expr) result-expr}].
}


@section{Defining New Types}

@defform/subs[{define-type name type-case ...+ type-option ...}
              ([type-case enum-id
                (tuple-id field-id ...)
                (record-id #:field-kw ...)]
               [type-option (code:line #:inspector inspector)
                (code:line #:property property expr)])
              #:contracts ([inspector inspector?]
                           [property struct-type-property?])]{
 Defines a new algebraic data type and binds the following names:

 @itemlist[
 @item{@racket[name]@tt{?} is bound to a predicate that returns @racket[#true], if the given value is one of the @racket[type-case]s; @racket[#false], otherwise;}
 @item{each @racket[enum-id] is bound to a unique value that is equal only to itself;}
 @item{each @racket[tuple-id] is bound to static information about the respective tuple case; it is also bound to a match-expander for constructing and destructuring values of the tuple case;}
 @item{for each @racket[tuple-id], the name @racket[tuple-id]@tt{?} is bound to a predicate that returns @racket[#true], if the given value is an instance of @racket[tuple-id]; @racket[#false], otherwise;}
 @item{for each @racket[tuple-id], the name @racket[tuple-id]@tt{-}@racket[field-id] is bound to a projection operation (accessor) for the respective field;}
 @item{each @racket[record-id] is bound to static information about the respective record case; it is alsso bound too a match-expander for constructing and destructuring values of the record case;}
 @item{for each @racket[record-id], the namme @racket[record-id]@tt{?} is bound to a predicate that returns @racket[#true], if the given value is an instance of @racket[record-id]; @racket[#false], otherwise;}
 @item{for each @racket[record-id], the name @racket[record-id]@tt{-}@racket[field-kw] is bound to a projection operation (accessor)  for the respective field.}
 ]
}
