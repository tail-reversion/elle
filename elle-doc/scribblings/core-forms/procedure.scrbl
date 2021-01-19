#lang scribble/manual

@(require "../util.rkt")


@title{Procedures}

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
