#lang racket/base

(require racket/generic
         racket/sequence)


(define-generics collection
  (in-collection collection)
  (empty? collection)
  (contains? collection element)
  (contains-any? collection sequence)
  (contains-all? collection sequence)
  (size collection))
