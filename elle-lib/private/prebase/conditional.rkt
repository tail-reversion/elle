#lang racket/base

(provide (rename-out [@%if if] [@%cond cond] [@%when when] [@%unless unless]))

(require (for-syntax racket/base)
         syntax/parse/define
         elle/private/prebase/preprebase)


(define-syntax-parser @%if
    #:track-literals
    [(_ {~describe "test expression" pred}
        {~describe "then expression" then:expr}
        ~!
        {~describe "else expression" else:expr})
     #:declare pred (expr/c #'boolean? #:name "test expression")
     #'(if pred.c then else)]
    [(_ {~describe "test expression" pred}
        {~describe "then expression with keyword" {~seq #:then then:expr}}
        ~!
        {~describe "else expression with keyword" {~seq #:else else:expr}})
     #:declare pred (expr/c #'boolean? #:name "test expression")
     #'(if pred.c then else)])


{begin-for-syntax
  (define-splicing-syntax-class cond-clause
    #:description "cond clause"
    #:attributes (contracted-test result)
    #:literals (⇒)
    [pattern {~seq test ⇒ result:expr}
             #:declare test (expr/c #'boolean? #:name "cond test expression")
             #:attr contracted-test #'test.c])
  }

(define-syntax-parser @%cond
    #:track-literals
    [(_ clause:cond-clause ...+ {~optional {~seq #:else final:expr}})
     #'(cond [clause.contracted-test clause.result] ...
             [else {~? final (error 'cond "no successful test clause")}])])


(define-syntax-parser @%when
    #:track-literals
    [(_ {~describe "test expression" pred}
        {~describe "guarded expression" does:expr})
     #:declare pred (expr/c #'boolean? #:name "test expression")
     #'(@if pred.c does (values))])


(define-syntax-parser @%unless
    #:track-literals
    [(_ {~describe "test expression" pred}
        {~describe "guarded expression" does:expr})
     #:declare pred (expr/c #'boolean? #:name "test expression")
     #'(@if pred.c (values) does)])
