#lang scribble/manual

@(require "util.rkt")


@title{Core Syntactic Forms}

Elle differs from other Lisps in that it uses several delimiters, each having a different meaning. Elle recognizes parentheses @racket[()], brackets @racket[[]], and braces @racket[{}] as delimiters. A syntactic form may assign arbitrary meaning to the delimiters, but each has an independent meaning, when used as part of a standalone expression:

@itemlist[
 @item{parentheses are used for procedure application;}
 @item{brackets are (currently) not used for anything and will result in a syntax error;}
 @item{braces are used for “syntax application” (special forms and macros).}
 ]


@section{Definitions}

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


@section{Patterns}

@defform*[#:link-target? #f #:id match-pat
          [literal
           id
           {derived stx ...}]
          #:contracts ([derived match-expander?])]{
 @emph{There is currently not a good story on patterns and their grammar.}
}

@defform*[#:link-target? #f #:id vals-pat #:literals (values)
          [(values pat ...)
           pat]]{
 The first form matches a multiple-values return and destructures the values using the provided @racket[pat]s.

 The second form is equivalent to @racket[(values pat)].
}


@section{Pattern Matching}

@defform/subs[#:literals (⇒)
              {match expr match-clause ...+}
              ([match-clause (code:line pat ⇒ result-expr)
                (code:line pat #:guarded [guard-clause ...+])]
               [guard-clause (code:line test-expr ⇒ result-expr)])
              #:contracts ([test-expr boolean?])]{

}


@defform/subs[#:literals (⇒)
              {match* [expr ...+] match*-clause ...+}
              ([match*-clause (code:line [pat ...+] ⇒ result-expr)
                (code:line [pat ...+] #:guarded [guard-clause ...+])])]{

}


@section{Procedures}

@defform*[#:literals (⇒)
          ({λ args ⇒ expr}
           {λ args ⇒ λ args ⇒ ... ... ... expr})]{
 The first form creates an anonymous procedure that is closed over its lexical environment.

 The second form is syntactic sugar for nested @racket[λ] forms, yielding a, “curried @racket[λ]”.
}

@defform/subs[#:link-target? #f #:id application
              (proc-expr arg ... maybe-rest)
              ([arg expr
                (code:line #:<kw> expr)]
               [maybe-rest (code:line)
                (code:line rest-expr #,ellipsis)])
              #:contracts ([proc-expr procedure?]
                           [rest-expr list?])]{
 As a standalone expression, a parenthesized form always refers to procedure application.
}


@section{Local Binding}

@defform[{let definition ...+ #:in expr}]{
 Performs the given definitions and makes available their bindings in the body of @racket[expr], which is evaluated in tail position, with respect to the @racket[let] form.
}


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
