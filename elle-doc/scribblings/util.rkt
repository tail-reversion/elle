#lang scribble/manual

@(provide (for-label (all-from-out elle))
          ellipsis
          deftype
          defcon)

@(require (for-syntax racket/base
                      rebellion/type/tuple/binding
                      rebellion/type/record/binding
                      rebellion/type/enum/binding)
          syntax/parse/define
          scribble/manual
          (for-label elle))


@(define ellipsis (racket ...))


@(define-syntax-parser deftype
   #:track-literals
   [(_ name:id preflow ...) #'(defform #:kind "type" #:id name name preflow ...)])

@(define-syntax-parser defcon
   #:track-literals
   [(_ tuple:tuple-id preflow ...) #'(defsubform #:kind "constructor" #:id tuple.name (tuple.name tuple.field-name ...) preflow ...)]
   [(_ record:record-id preflow ...) #'(defsubform #:kind "constructor" #:id record.name (record.name record.field-keyword ...))]
   [(_ enum:id preflow ...) #'(defsubform #:kind "constructor" #:id enum enum preflow ...)])
