#lang elle/private/prebase

{provide parameter?
         make-parameter
         parameterize
         parameterize*
         parameter-procedure=?
         current-parameterization
         call-with-parameterization
         parameterization?}

{require-for-syntax racket/base #:exposing-all}
{require syntax/parse/define #:exposing-all}
{require elle/private/symbol #:exposing symbol?}
{require elle/private/option #:exposing-all}
{require racket/base
         #:exposing
         parameter?
         parameter-procedure=?
         make-parameter #:as rkt:make-parameter
         parameterize #:as rkt:parameterize
         parameterize* #:as rkt:parameterize*
         current-parameterization
         call-with-parameterization
         parameterization?}


make-parameter : {λ/c any/c [#:name symbol? #:guard (option/c {λ/c any/c → #:unconstrained})] → parameter?}
(make-parameter v [#:name name = 'parameter-procedure #:guard guard = absent]) ⇒
(rkt:make-parameter v (option-get guard #false) name)

#(define-syntax-parser parameterize
   #:track-literals
   #:literals (←)
   [(_ {~seq param ← val:expr} ...+ #:in body:expr)
    #:declare param (expr/c #'parameter?)
    #'(rkt:parameterize ([param.c val] ...)
                        body)])

#(define-syntax-parser parameterize*
   #:track-literals
   #:literals (←)
   [(_ {~seq param ← val:expr} ...+ #:in body:expr)
    #:declare param (expr/c #'parameter?)
    #'(rkt:parameterize* ([param.c val] ...)
                         body)])
