#lang racket/base

(require racket/contract)

(provide gen:collection
         (contract-out
          [in-collection (-> collection? (sequence/c any/c))]
          [empty? (-> collection? boolean?)]
          [contains? (-> collection? any/c boolean?)]))

(require racket/generic
         racket/sequence
         rebellion/collection/vector
         (only-in racket/vector vector-empty?)
         rebellion/collection/immutable-vector)


(define-generics collection
  (in-collection collection)
  (empty? collection)
  (contains? collection element)
  (size collection)
  #:fast-defaults ([list? (define in-collection in-list)
                          (define empty? null?)
                          (define (contains? lst elt)
                            (if (member lst elt) #t #f))
                          (define size length)]
                   [immutable-vector? (define in-collection in-vector)
                                      (define empty? empty-immutable-vector?)
                                      (define (contains? immvec elt)
                                        (if (immutable-vector-member immvec elt) #t #f))
                                      (define size immutable-vector-length)]
                   [mutable-vector? (define in-collection in-vector)
                                      (define empty? vector-empty?)
                                      (define (contains? mvec elt)
                                        (if (immutable-vector-member elt) #t #f))
                                      (define size vector-length)]))
