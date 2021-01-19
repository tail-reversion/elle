#lang scribble/manual

@(require "../util.rkt")


@title{Patterns and Matching}

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
