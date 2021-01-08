#lang racket/base

(provide (for-label (all-from-out elle))
         ellipsis)

(require (for-syntax racket/base syntax/parse)
         scribble/manual
         (for-label elle))


(define ellipsis (racket ...))


(define-syntax (reexport-note stx)
  (syntax-parse stx
    [(_ name:id lib:id) #'()]))

(define-syntax (defstxfrag stx)
  (syntax-parse stx
    [(_ name:id ([nonterm:id gramm] ...+) preflow ...)
     #'(defform #:kind "syntax fragment" #:id name #:literals (name)
         ([nonterm gramm] ...)
         preflow ...)]))
