#lang racket/base

(provide define-type
         (rename-out
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


{begin-for-syntax
  (define-record-type tuple-type-desc (name predicate-name fields))

  (define-record-type record-type-desc (name predicate-name fields))

  (define-wrapper-type enum-case)
  
  (define-record-type algebraic-type (name predicate tuple-bindings record-bindings enum-binding))
  
  (define-syntax-class type-case-declaration
    #:description "type case"
    #:attributes (type)
    [pattern (name:id field:id ...+)
             #:attr type (tuple-type-desc #:name #'name
                                          #:predicate-name (format-id #'name "~a?" #'name)
                                          #:fields (datum (field ...)))]
    [pattern (name:id field:keyword ...+)
             #:attr type (record-type-desc #:name #'name
                                           #:predicate-name (format-id #'name "~a?" #'name)
                                           #:fields (map (compose string->symbol keyword->string syntax-e)
                                                         (datum (field ...))))]
    [pattern name:id
             #:attr type (enum-case #'name)])

  (define-splicing-syntax-class type-case-declaration-set
    #:description "type case declarations"
    #:attributes (tuples records enum-cases)
    [pattern {~seq tycase:type-case-declaration ...+}
             #:attr tuples (filter tuple-type-desc? (datum (tycase.type ...)))
             #:attr records (filter record-type-desc? (datum (tycase.type ...)))
             #:attr enum-cases (map enum-case-value (filter enum-case? (datum (tycase.type ...))))])
  }

(define-syntax-parser define-type
  #:track-literals
  [(_ name:id
      tycases:type-case-declaration-set
      {~alt {~optional {~seq #:inspector inspector-expr}
                       #:name "#:inspector option"}
            {~seq #:property prop prop-val:expr}} ...)
   #:declare inspector-expr (expr/c #'inspector?)
   #:declare prop (expr/c #'struct-type-property?)
   #:with inspector #'{~? inspector-expr.c (current-inspector)}
   #:with pred-name (format-id #'name "~a?" #'name)
   #:with enum-name (format-id #'name "~a:enum" #'name)
   #:with enum-pred-name (format-id #'enum-name "~a?" #'enum-name)
   #:with (tuple-names ...) (map tuple-type-desc-name (attribute tycases.tuples))
   #:with (record-names ...) (map record-type-desc-name (attribute tycases.records))
   #:with (pred-names ...) (cons #'enum-pred-name (append (map tuple-type-desc-predicate-name (attribute tycases.tuples))
                                                          (map record-type-desc-predicate-name (attribute tycases.records))))
   #:with (prop-val-name ...) (generate-temporaries (datum (prop ...)))
   #`(begin (define prop-val-name prop-val) ...
            (define-enum-type enum-name #,(attribute tycases.enum-cases)
              #:predicate-name enum-pred-name
              #:inspector inspector)
            #,@(for/list ([tuple-ty (attribute tycases.tuples)])
                 #`(define-tuple-type #,(tuple-type-desc-name tuple-ty) #,(tuple-type-desc-fields tuple-ty)
                     #:predicate-name #,(tuple-type-desc-predicate-name tuple-ty)
                     #:inspector inspector
                     #:property-maker (λ (desc)
                                        (append (list (cons prop prop-val-name) ...)
                                                (default-tuple-properties desc)))))
            #,@(for/list ([record-ty (attribute tycases.records)])
                 #`(define-record-type #,(record-type-desc-name record-ty) #,(record-type-desc-fields record-ty)
                     #:predicate-name #,(record-type-desc-predicate-name record-ty)
                     #:inspector inspector))
            (define pred-name (λ (x) (or (pred-names x) ...)))
            (define-syntax name (algebraic-type #:name #'name
                                                #:predicate #'pred-name
                                                #:tuple-bindings (list #'tuple-names ...)
                                                #:record-bindings (list #'record-names ...)
                                                #:enum-binding #'enum-name)))])



; test case
#;(require racket/match)
#;(define-type one-or-two
    (one-proc val)
    (two-procs val1 val2)
    #:property prop:procedure (λ (self a b)
                                (match self
                                  [(one-proc f) (f a b)]
                                  [(two-procs f g) (f (g a b))])))
