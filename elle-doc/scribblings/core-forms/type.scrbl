#lang scribble/manual

@(require "../util.rkt")


@title{Defining New Types}

@section{Algebraic Data Types}

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
 @item{each @racket[record-id] is bound to static information about the respective record case; it is also bound to a match-expander for constructing and destructuring values of the record case;}
 @item{for each @racket[record-id], the name @racket[record-id]@tt{?} is bound to a predicate that returns @racket[#true], if the given value is an instance of @racket[record-id]; @racket[#false], otherwise;}
 @item{for each @racket[record-id], the name @racket[record-id]@tt{-}@racket[field-kw] is bound to a projection operation (accessor) for the respective field.}
 ]
}


@section{Wrapper Types}

@defform/subs[{define-wrapper-type name type-option ...}
              ([type-option (code:line #:inspector inspector)
                (code:line #:property property expr)])
              #:contracts ([inspector inspector?]
                           [property struct-type-property?])]{

}


@section{Object Types}

@defform/subs[{define-object-type name (#:field-kw ...) type-option}
              ([type-option (code:line #:inspector inspector)
                (code:line #:property property expr)])
              #:contracts ([inspector inspector?]
                           [property struct-type-property?])]{

}
