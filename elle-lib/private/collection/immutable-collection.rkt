#lang racket/base

(require racket/contract)

(provide gen:immutable-collection
         (contract-out
          [immutable-collection? (-> any/c boolean?)]
          [add (-> immutable-collection? any/c immutable-collection?)]
          [add-all (-> immutable-collection? (sequence/c any/c) immutable-collection?)]
          [remove (-> immutable-collection? any/c immutable-collection?)]
          [remove-all (-> immutable-collection? (sequence/c any/c) immutable-collection?)]))

(require racket/generic
         racket/sequence
         elle/private/collection/collection)


(define-generics immutable-collection
  (add immutable-collection element)
  (add-all immutable-collection sequence)
  (remove immutable-collection element)
  (remove-all immutable-collection sequence))
