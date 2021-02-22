#lang racket/base

(provide (rename-out [@%λ λ]
                     [@%let let])
         (for-syntax procedure-header))

(require (for-syntax racket/base syntax/datum)
         syntax/parse/define
         elle/private/prebase/preprebase)


{begin-for-syntax
  (define-splicing-syntax-class mandatory-positional-arguments
    #:description "mandatory positional arguments"
    #:attributes ([names 1] [normalized 1])
    [pattern {~seq names:id ...}
             #:attr (normalized 1) (datum (names ...))])

  (define-splicing-syntax-class mandatory-keyword-arguments
    #:description "mandatory keyword arguments"
    #:attributes ([names 1] [normalized 1])
    [pattern {~seq {~seq kw:keyword names:id} ...}
             #:attr (normalized 1) (datum ({~@ kw names} ...))])

  (define-splicing-syntax-class mandatory-arguments
    #:description "mandatory arguments"
    #:attributes ([names 1] [normalized 1])
    [pattern {~seq args:mandatory-positional-arguments kwargs:mandatory-keyword-arguments}
             #:attr (names 1) (datum (args.names ... kwargs.names ...))
             #:attr (normalized 1) (datum (args.normalized ... kwargs.normalized ...))])

  
  (define-splicing-syntax-class optional-positional-arguments
    #:description "optional positional arguments"
    #:attributes ([names 1] [normalized 1])
    #:literals (=)
    [pattern {~seq {~seq names:id = default:expr} ...}
             #:attr (normalized 1) (syntax->list #'([names default] ...))])

  (define-splicing-syntax-class optional-keyword-arguments
    #:description "optional keyword arguments"
    #:attributes ([names 1] [normalized 1])
    #:literals (=)
    [pattern {~seq {~seq kw:keyword names:id = default:expr} ...}
             #:attr (normalized 1) (syntax->list #'({~@ kw [names default]} ...))])

  (define-syntax-class optional-arguments
    #:description "optional arguments"
    #:attributes ([names 1] [normalized 1])
    #:literals (#%brackets)
    [pattern (#%brackets args:optional-positional-arguments kwargs:optional-keyword-arguments)
             #:attr (names 1) (datum (args.names ... kwargs.names ...))
             #:attr (normalized 1) (datum (args.normalized ... kwargs.normalized ...))])

  
  (define-splicing-syntax-class arguments
    #:description "arguments"
    #:attributes ([names 1] [normalized 1])
    [pattern {~seq margs:mandatory-arguments}
             #:attr (names 1) (datum (margs.names ...))
             #:attr (normalized 1) (datum (margs.normalized ...))
             #:attr rest-only? #f]
    [pattern {~seq margs:mandatory-arguments oargs:optional-arguments}
             #:attr (names 1) (datum (margs.names ... oargs.names ...))
             #:attr (normalized 1) (datum (margs.normalized ... oargs.normalized ...))
             #:attr rest-only? #f])

  (define-splicing-syntax-class arguments-maybe-rest
    #:attributes ([contents 1])
    [pattern {~seq args:arguments rest:id {~literal ...}}
             #:with ellipses #'{... ...}
             #:attr (contents 1) (datum ({~@ . args} rest ellipses))]
    [pattern {~seq args:arguments}
             #:attr (contents 1) (datum ({~@ . args}))])


  
  (define-syntax-class procedure-header
    #:description "procedure header"
    #:attributes (name [curried-λ-heads 1])
    #:literals (#%parens)
    [pattern (#%parens name:safe-id args:arguments-maybe-rest)
             #:attr (curried-λ-heads 1) (list #'(@%λ args.contents ... ⇒))]
    [pattern (#%parens header:procedure-header args:arguments-maybe-rest)
             #:attr name (attribute header.name)
             #:attr (curried-λ-heads 1) (append (attribute header.curried-λ-heads) (list #'(@%λ args.contents ... ⇒)))])
  }


(define-syntax-parser @%λ
  #:track-literals
  #:literals (⇒)
  [(_ args:arguments-maybe-rest ⇒ {~seq {~literal @%λ} other-args:arguments-maybe-rest ⇒} ...+ body:safe-expr)
   (syntax/loc this-syntax
     (simple-λ args.contents ... ⇒ {{~@ @%λ other-args.contents ... ⇒} ... body}))]
  [(_ args:arguments-maybe-rest ⇒ body:safe-expr)
   (syntax/loc this-syntax
     (simple-λ args.contents ... ⇒ body))])

(define-syntax-parser simple-λ
  #:track-literals
  #:literals (⇒)
  [(_ rest:safe-id {~literal ...} ⇒ body:safe-expr)
   (syntax/loc this-syntax
     (λ rest body))]
  [(_ args:arguments rest:id {~literal ...} ⇒ body:safe-expr)
   (syntax/loc this-syntax
     (λ (args.normalized ... . rest) body))]
  [(_ args:arguments ⇒ body:safe-expr)
   (syntax/loc this-syntax
     (λ (args.normalized ...) body))])




{begin-for-syntax
  (define-splicing-syntax-class let-binding
    #:description "let binding"
    #:attributes (normalized)
    #:literals (= ⇒)
    [pattern {~seq name:id = e:safe-expr}
             #:attr normalized #'(name e)]
    [pattern {~seq header:procedure-header ⇒ body:safe-expr}
             #:attr normalized #'((header.name) ({~@ . header.curried-λ-heads} ... body))])
  }

(define-syntax-parser @%let
  #:track-literals
  #:literals (#%brackets =)
  [(_ lb:let-binding ...+ #:in body:safe-expr)
   (syntax/loc this-syntax
     (letrec (lb.normalized ...)
       body))])
