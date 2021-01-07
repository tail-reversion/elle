#lang racket/base

(provide : = ← → ⇒
         (for-syntax safe-id safe-expr)
         #%parens #%brackets #%braces)

(require (for-syntax racket/base syntax/datum)
         syntax/parse/define
         (only-in racket/match match-expander?))

(module+ app
  (provide (rename-out [@%app #%app])))


{begin-for-syntax
  (struct syntax-literal ()
    #:property prop:set!-transformer (λ (self stx)
                                       (raise-syntax-error #f "not allowed as an expression" stx)))

  (define-syntax-class safe-id
    #:description "non-literal id"
    #:attributes ()
    [pattern {~and _:id
                   {~not {~var _ (static syntax-literal? #f)}}}])

  (define-syntax-class safe-expr
    #:description "expr that is not a literal"
    #:attributes ()
    [pattern {~and _:expr
                   {~not {~var _ (static syntax-literal? #f)}}}])
  }

(define-syntax-parser define-syntax-literal
  #:track-literals
  [(_ lit:id) #'(define-syntax lit (syntax-literal))])


(define-syntax-literal :)

(define-syntax-literal =)

(define-syntax-literal ←)

(define-syntax-literal →)

(define-syntax-literal ⇒)



(define-syntax-parser #%parens
  #:track-literals
  [(_ stuff ...) (syntax/loc this-syntax
                   (@%app stuff ...))])


(define-syntax (#%brackets stx)
  (raise-syntax-error #f "not for use by itself" stx))


(define-syntax-parser #%braces
  #:track-literals
  [(_ stuff ...) (syntax/loc this-syntax
                   (@%stx-app stuff ...))])

{begin-for-syntax
  (define-syntax-class syntactic-form
    #:description "syntactic form"
    #:attributes ()
    #:opaque
    [pattern {~var name (static (λ (val)
                                  (and (procedure? val)
                                       (equal? 1 (procedure-arity val))))
                                "macro")}]
    [pattern {~var name (static match-expander? "match expander")}])
  }

(define-syntax-parser @%stx-app
  #:track-literals
  #:context 'syntax-application
  [(_ f:syntactic-form other-stx ...) (syntax/loc this-syntax
                                        (f other-stx ...))])


{begin-for-syntax
  (define-splicing-syntax-class argument
    #:description "procedure argument"
    #:attributes (keyword?)
    [pattern e:expr
             #:attr keyword? #f]
    [pattern {~seq kw:keyword e:expr}
             #:attr keyword? #t])
  }

(define-syntax-parser @%app
  #:track-literals
  [(_ proc:expr arg:argument ... rest {~literal ...})
   #:declare rest (expr/c #'list? #:name "rest argument")
   #:with ((val-arg) ...) (for/list ([kw? (in-list (datum (arg.keyword? ...)))]
                                     #:unless kw?
                                     [arg (in-list (datum (arg ...)))])
                            arg)
   #:with (kw-arg ...) (for/list ([kw? (in-list (datum (arg.keyword? ...)))]
                                  #:when kw?
                                  [arg (in-list (datum (arg ...)))])
                         arg)
   (syntax/loc this-syntax
     (apply proc val-arg ... rest.c {~@ . kw-arg} ...))]
  [(_ proc:expr arg:argument ...)
   (syntax/loc #'proc
     (#%app proc {~@ . arg} ...))])
