#lang racket/base

(provide module
         module*
         module+
         #%expression
         #%top
         #%variable-reference
         #%top-interaction
         (rename-out
          [@%quote quote]
          [@%datum #%datum]))

(require (for-syntax racket/base syntax/parse)
         syntax/parse/define
         elle/private/prebase/preprebase)


(define-syntax-parser @%quote
    #:track-literals
    [(_ id:id) #''id]
    [(_ kw:keyword) #''kw]
    [(_ datum) (raise-syntax-error #f "only symbols and keywords may be quoted" this-syntax)])


(define-syntax-parser @%datum
    #:track-literals
    [(_ . stuff)
     (if (prefab-struct-key (syntax-e #'stuff))
         (raise-syntax-error #f "illegal struct syntax" #'stuff)
         #''stuff)])
