#lang racket/base

(provide (for-syntax match-pattern values-pattern match-clause match*-clause)
         (rename-out [@%match/derived match/derived]
                     [@%match match]
                     [@%match*/derived match*/derived]
                     [@%match* match*]))

(require (for-syntax racket/base syntax/datum)
         syntax/parse/define
         racket/match
         elle/private/prebase/preprebase)


{begin-for-syntax
  (define-syntax-class literal-pattern
    #:description "literal datum pattern"
    #:attributes ()
    [pattern _:string]
    [pattern _:bytes]
    [pattern _:number]
    [pattern _:boolean]
    [pattern _:character]
    [pattern ({~literal quote} _:id)])

  
  (define-syntax-class match-pattern
    #:description "matching pattern"
    #:attributes (normalized)
    #:local-conventions ([pat match-pattern])
    #:literals (#%braces and or not)
    [pattern {~literal _}
             #:attr normalized #'_]
    [pattern v:safe-id
             #:attr normalized #'v]
    [pattern lit:literal-pattern
             #:attr normalized #'lit]
    [pattern (#%braces and pat ...)
             #:attr normalized #'(and pat.normalized ...)]
    [pattern (#%braces or pat ...)
             #:attr normalized #'(or pat.normalized ...)]
    [pattern (#%braces not pat ...)
             #:attr normalized #'(not pat.normalized ...)]
    [pattern (#%braces {~var other (static match-expander? "match expander")} stx ...)
             #:with (sneaky-stx ...) (map sneaky-strip-braces (datum (stx ...)))
             #:attr normalized #'(other sneaky-stx ...)])

  (define sneaky-strip-braces
    (syntax-parser
      #:track-literals
      #:literals (#%braces)
      [(#%braces {~var name (static match-expander? #f)} stx ...)
       #'(name stx ...)]
      [other #'other]))
  

  (define-syntax-class values-pattern
    #:description "values pattern"
    #:attributes ([normalized 1])
    #:literals (#%parens values)
    [pattern (#%parens values pat:match-pattern ...)
             #:attr (normalized 1) (datum (pat.normalized ...))]
    [pattern pat:match-pattern
             #:attr (normalized 1) (list #'pat.normalized)])


  (define-splicing-syntax-class guard-clause
    #:description "guard clause"
    #:attributes (contracted-test result)
    #:literals (⇒)
    [pattern {~seq test ⇒ result:expr}
             #:declare test (expr/c #'boolean?)
             #:attr contracted-test #'test.c])


  (define-splicing-syntax-class match*-clause
    #:description "match* clause"
    #:attributes (normalized)
    #:literals (#%brackets ⇒)
    [pattern {~seq (#%brackets pat:match-pattern ...+) ⇒ body:expr}
             #:attr normalized #'((pat.normalized ...) body)]
    [pattern {~seq (#%brackets pat:match-pattern ...+) #:guarded (#%brackets cls:guard-clause ...+)}
             #:attr normalized #'((pat.normalized ...) (cond [cls.contracted-test cls.result] ...
                                                             [else (failure-cont)]))])

  (define-splicing-syntax-class match-clause
    #:description "match clause"
    #:attributes (normalized)
    #:literals (#%brackets ⇒)
    [pattern {~seq pat:match-pattern ⇒ body:expr}
             #:attr normalized #'(pat.normalized body)]
    [pattern {~seq pat:match-pattern #:guarded (#%brackets cls:guard-clause ...+)}
             #:attr normalized #'(pat.normalized (cond [cls.contracted-test cls.result] ...
                                                       [else (failure-cont)]))])
  }


(define-syntax-parser @%match/derived
  #:track-literals
  [(_ #:form-name name:id e:expr clause:match-clause ...+)
   (syntax/loc this-syntax
     (match/derived e '(name) clause.normalized ...))])

(define-syntax-parser @%match
  #:track-literals
  [(_ e:expr clause:match-clause ...+)
   (syntax/loc this-syntax
     (@%match/derived #:form-name match e {~@ . clause} ...))])

(define-syntax-parser @%match*/derived
  #:track-literals
  #:literals (#%brackets)
  [(_ #:form-name name:id (#%brackets e:expr ...+) clause:match*-clause ...+)
   (syntax/loc this-syntax
     (match*/derived (e ...) '(name) clause.normalized ...))])

(define-syntax-parser @%match*
  #:track-literals
  #:literals (#%brackets)
  [(_(#%brackets e:expr ...+) clause:match*-clause ...+)
   (syntax/loc this-syntax
     (@%match*/derived #:form-name match* (#%brackets e ...) {~@ . clause} ...))])
