#lang racket/base

(require racket/contract)

(provide parameter?
         (contract-out [rename @%make-parameter make-parameter (->* (any/c) (#:name symbol? #:guard (or/c (-> any/c any) #f)) parameter?)])
         (rename-out [@%parameterize parameterize]
                     [@%parameterize* parameterize*])
         make-derived-parameter
         parameter-procedure=?
         current-parameterization
         call-with-parameterization
         parameterization?)

(require (for-syntax racket/base)
         syntax/parse/define
         elle/private/prebase/preprebase)


(define (@%make-parameter v #:name [name 'parameter-procedure] #:guard [guard #f])
  (make-parameter v guard name))

(define-syntax-parser @%parameterize
    #:track-literals
    #:literals (←)
    [(_ {~seq param ← val:expr} ...+ #:in body:expr)
     #:declare param (expr/c #'parameter?)
     #'(parameterize ([param.c val] ...)
         body)])

(define-syntax-parser @%parameterize*
    #:track-literals
    #:literals (←)
    [(_ {~seq param ← val:expr} ...+ #:in body:expr)
     #:declare param (expr/c #'parameter?)
     #'(parameterize* ([param.c val] ...)
         body)])
