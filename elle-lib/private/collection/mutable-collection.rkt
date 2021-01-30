#lang racket/base

(require racket/contract)

(provide gen:mutable-collection
         (contract-out
          [mutable-collection? (-> any/c boolean?)]
          [add! (-> mutable-collection? any/c mutable-collection?)]
          [add-all! (-> mutable-collection? (sequence/c any/c) mutable-collection?)]
          [remove! (-> mutable-collection? any/c mutable-collection?)]
          [remove-all! (-> mutable-collection? (sequence/c any/c) mutable-collection?)]))

(require racket/generic
         racket/sequence
         elle/private/collection/collection)


(define-generics mutable-collection
  (add! mutable-collection element)
  (add-all! mutable-collection sequence)
  (remove! mutable-collection element)
  (remove-all! mutable-collection sequence))
