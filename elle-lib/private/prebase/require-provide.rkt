#lang racket/base

(provide @%require require-for-syntax require-for-template require-for-label
         @%provide provide-for-syntax provide-for-template provide-for-label
         reprovide)

(require (for-syntax racket/base syntax/datum racket/set)
         syntax/parse/define
         racket/contract
         elle/private/prebase/preprebase
         (only-in racket/require filtered-in)
         (only-in racket/provide filtered-out)
         (only-in (submod elle/private/prebase/module-body secret-provide) lookup-contract))


{begin-for-syntax
  (define-syntax-class root-module-path
    #:description "root module path"
    [pattern sym-path:id
             #:fail-unless ((compose module-path? syntax-e) #'sym-path)
             "invalid symbolic path"]
    [pattern str-path:str
             #:fail-unless ((compose module-path? syntax-e) #'str-path)
             "invalid string path"])

  (define-splicing-syntax-class module-path
    #:description "module path"
    #:attributes (normalized)
    #:literals (#%brackets)
    [pattern rm:root-module-path
             #:attr normalized #'rm]
    [pattern {~seq rm:root-module-path #:submodule (#%brackets part:id ...)}
             #:attr normalized #'(submod rm part ...)])

  (define-splicing-syntax-class maybe-rename
    #:attributes (normalized)
    [pattern {~seq old-name:id #:as new-name:id}
             #:attr normalized #'(old-name new-name)]
    [pattern name:id
             #:attr normalized #'name])
  
  (define-splicing-syntax-class require-form
    #:description "require form"
    #:attributes (normalized)
    #:literals (#%brackets)
    [pattern {~seq target:module-path #:with-prefix prefix:id}
             #:attr normalized #'(prefix-in prefix target.normalized)]
    [pattern {~seq target:module-path #:exposing mr:maybe-rename ...+}
             #:attr normalized #'(only-in target.normalized mr.normalized ...)]
    [pattern {~seq target:module-path #:exposing-all}
             #:attr normalized #'target.normalized]
    [pattern {~seq target:module-path #:hiding name:id ...}
             #:attr normalized #'(except-in target.normalized name ...)])
  }

(define-syntax-parser @%require
  #:track-literals
  [(_ #:phase n:exact-integer in:require-form) (syntax/loc this-syntax
                                                 (require (for-meta n in.normalized)))]
  [(_ in:require-form) (syntax/loc this-syntax
                         (require in.normalized))])

(define-syntax-parser require-for-syntax
  #:track-literals
  [(_ in:require-form) (syntax/loc this-syntax
                         (@%require #:phase 1 {~@ . in}))])

(define-syntax-parser require-for-template
  #:track-literals
  [(_ in:require-form) (syntax/loc this-syntax
                         (@%require #:phase -1 {~@ . in}))])

(define-syntax-parser require-for-label
  #:track-literals
  [(_ in:require-form) (syntax/loc this-syntax
                         (require (for-label in.normalized)))])


{begin-for-syntax
  (define-splicing-syntax-class provide-form
    #:description "provide form"
    #:attributes (renamed? contracted? normalized)
    [pattern name:id
             #:attr renamed? #f
             #:attr contracted? (if (lookup-contract #'name) #t #f)
             #:attr normalized (if (attribute contracted?) #`(name #,(lookup-contract #'name)) #'name)]
    [pattern {~seq old-name:id #:as new-name:id}
             #:attr renamed? #t
             #:attr contracted? (if (lookup-contract #'name) #t #f)
             #:attr normalized (if (attribute contracted?)
                                   #`(rename old-name new-name #,(lookup-contract #'old-name))
                                   #'(old-name new-name))])

  (define-splicing-syntax-class provides
    #:description "provides"
    #:attributes ([uncontracted 1] [uncontracted-renamed 1] [contracted 1])
    [pattern {~seq pf:provide-form ...}
             #:do [(define contracted? (datum (pf.contracted? ...)))
                   (define renamed? (datum (pf.renamed? ...)))
                   (define normalized (datum (pf.normalized ...)))]
             #:attr (uncontracted 1)
             (for/list ([c? (in-list contracted?)]
                        [r? (in-list renamed?)]
                        [p (in-list normalized)]
                        #:when (and (not c?) (not r?)))
               p)
             #:attr (uncontracted-renamed 1)
             (for/list ([c? (in-list contracted?)]
                        [r? (in-list renamed?)]
                        [p (in-list normalized)]
                        #:when (and (not c?) r?))
               p)
             #:attr (contracted 1)
             (for/list ([c? (in-list contracted?)]
                        [p (in-list normalized)]
                        #:when c?)
               p)])
  }


(define-syntax-parser @%provide
  #:track-literals
  [(_ #:phase n:exact-integer out:provides)
   (syntax-local-lift-module-end-declaration
    #'(provide-aux n {~@ . out}))
   #'(begin)]
  [(_ out:provides)
   (syntax-local-lift-module-end-declaration
    #'(provide-aux 0 {~@ . out}))
   #'(begin)])

(define-syntax-parser provide-for-syntax
  #:track-literals
  [(_ out:provides) #'(@%provide #:phase 1 {~@ . out})])

(define-syntax-parser provide-for-template
  #:track-literals
  [(_ out:provides) #'(@%provide #:phase -1 {~@ . out})])

(define-syntax-parser provide-for-label
  #:track-literals
  [(_ out:provides) #'(provide-aux #f {~@ . out})])

(define-syntax-parser provide-aux
  #:track-literals
  [(_ ph out:provides) #'(provide (for-meta ph
                                            out.uncontracted ...
                                            (rename-out out.uncontracted-renamed ...)
                                            (contract-out out.contracted ...)))])


{begin-for-syntax
  (define-splicing-syntax-class reprovide-form
    #:description  "reprovide form"
    #:attributes (normalized-modpath normalized-require)
    #:literals (#%brackets)
    [pattern {~seq modpath:module-path #:exposing-all}
             #:attr normalized-modpath #'modpath.normalized
             #:attr normalized-require #'modpath.normalized]
    [pattern {~seq modpath:module-path #:exposing mr:maybe-rename ...+}
             #:attr normalized-modpath #'modpath.normalized
             #:attr normalized-require #'(only-in modpath.normalized mr.normalized ...)]
    [pattern {~seq modpath:module-path #:hiding name:id ...+}
             #:attr normalized-modpath #'modpath.normalized
             #:attr normalized-require #'(except-in modpath.normalized name ...)])
  }

(define-syntax-parser reprovide
  #:track-literals
  [(_ rpf:reprovide-form)
   #:with reprovide-set (gensym)
   (syntax-local-introduce
    (syntax/loc this-syntax
      (begin (define-for-syntax reprovide-set (mutable-set))
             (require (filtered-in (λ (name)
                                     (set-add! reprovide-set name)
                                     name)
                                   rpf.normalized-require))
             (provide (filtered-out (λ (name)
                                      (and (set-member? reprovide-set name) name))
                                    (all-from-out rpf.normalized-modpath))))))])
