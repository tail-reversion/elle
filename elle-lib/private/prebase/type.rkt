#lang racket/base

(provide (rename-out
          [@%define-singleton-type define-singleton-type]
          [@%define-wrapper-type define-wrapper-type]
          [@%define-object-type define-object-type]))

(require (for-syntax racket/base racket/syntax syntax/datum)
         (for-syntax rebellion/type/record rebellion/type/enum rebellion/type/wrapper)
         syntax/parse/define
         rebellion/type/tuple
         rebellion/type/record
         rebellion/type/enum
         rebellion/type/singleton
         rebellion/type/wrapper
         rebellion/type/object)


(define-syntax-parser @%define-singleton-type
  #:track-literals
  [(_ name:id
      {~alt {~optional {~seq #:inspector inspector-expr}
                       #:name "#:inspector option"}
            {~seq #:property prop prop-val:expr}}...)
   #:declare inspector-expr (expr/c #'inspector?)
   #:declare prop (expr/c #'struct-type-property?)
   #:with inspector #'{~? inspector-expr.c (current-inspector)}
   #:with (prop-val-name ...) (generate-temporaries (datum (prop ...)))
   #'(begin (define prop-val-name prop-val) ...
            (define-singleton-type name
              #:inspector inspector
              #:property-maker (λ (desc)
                                 (append (list (cons prop prop-val-name) ...)
                                         (default-singleton-properties desc)))))])


(define-syntax-parser @%define-wrapper-type
  #:track-literals
  [(_ name:id
      {~alt {~optional {~seq #:inspector inspector-expr}
                       #:name "#:inspector option"}
            {~seq #:property prop prop-val:expr}} ...)
   #:declare inspector-expr (expr/c #'inspector?)
   #:declare prop (expr/c #'struct-type-property?)
   #:with inspector #'{~? inspector-expr.c (current-inspector)}
   #:with (prop-val-name ...) (generate-temporaries (datum (prop ...)))
   #'(begin (define prop-val-name prop-val) ...
            (define-wrapper-type name
              #:inspector inspector
              #:property-maker (λ (desc)
                                 (append (list (cons prop-val-name prop-val) ...)
                                         (default-wrapper-properties desc)))))])


(define-syntax-parser @%define-object-type
  #:track-literals
  [(_ name:id (field:keyword ...)
      {~alt {~optional {~seq #:inspector inspector-expr}
                       #:name "#:inspector option"}
            {~seq #:property prop prop-val:expr}} ...)
   #:declare inspector-expr (expr/c #'inspector?)
   #:declare prop (expr/c #'struct-type-property?)
   #:with (field-id ...) (map (compose string->symbol keyword->string syntax-e) (datum (field ...)))
   #:with inspector #'{~? inspector-expr.c (current-inspector)}
   #:with (prop-val-name ...) (generate-temporaries (datum (prop ...)))
   #'(begin (define prop-val-name prop-val) ...
            (define-object-type name (field-id ...)
              #:inspector inspector
              #:property-maker (λ (desc)
                                 (append (list (cons prop prop-val-name) ...)
                                         (default-object-properties desc)))))])


