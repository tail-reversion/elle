#lang racket/base

(require racket/sequence
         rebellion/type/record
         rebellion/type/tuple
         rebellion/collection/record
         rebellion/collection/immutable-vector
         (only-in racket/syntax format-symbol))



(define-tuple-type algebraic-type (name variants)
  #:omit-root-binding)

(define (algebraic-type name variants)
  (for ([name (record-keywords variants)]
        [fields (record-values variants)])
    (when (check-duplicate-identifier (sequence->list fields))
      (raise-argument-error 'duplicate-field-name "only unique field names" fields)))
  (constructor:algebraic-type name (record-map variants
                                               (λ (field-seq)
                                                 (vector->immutable-vector
                                                  (for/vector #:length (sequence-count field-seq)
                                                    ([field field-seq])
                                                    field))))))



(define-record-type uninitialized-algebraic-descriptor (type predicate constructors accessors enumerator))

(define-record-type initialized-algebraic-descriptor (type predicate constructors accessors enumerator backing-tuple-descriptor))

(define ((make-getter uninit init) desc)
  (if (initialized-algebraic-descriptor? desc)
      (init desc)
      (uninit desc)))

(define algebraic-descriptor.type
  (make-getter uninitialized-algebraic-descriptor-type
               initialized-algebraic-descriptor-type))

(define algebraic-descriptor.predicate
  (make-getter uninitialized-algebraic-descriptor-predicate
               initialized-algebraic-descriptor-predicate))

(define algebraic-descriptor.constructors
  (make-getter uninitialized-algebraic-descriptor-constructors
               initialized-algebraic-descriptor-constructors))

(define algebraic-descriptor.accessors
  (make-getter uninitialized-algebraic-descriptor-accessors
               initialized-algebraic-descriptor-accessors))

(define algebraic-descriptor.enumerator
  (make-getter uninitialized-algebraic-descriptor-enumerator
               initialized-algebraic-descriptor-enumerator))


(define (make-algebraic-implementation type #:inspector [inspector (current-inspector)])
  (define (tuple-property-maker descriptor)
    (default-algebraic-properties (tuple-descriptor->algebraic-descriptor descriptor type)))
  (define descriptor (make-tuple-implementation (algebraic-type->tuple-type type) #:inspector inspector #:property-maker tuple-property-maker))
  (tuple-descriptor->algebraic-descriptor descriptor type))

(define (tuple-descriptor->algebraic-descriptor descriptor type)
  (define predicate (tuple-descriptor-predicate descriptor))
  (define tuple-constructor (tuple-descriptor-constructor descriptor))
  (define tuple-accessor (tuple-descriptor-accessor descriptor))
  (define variants (algebraic-type-variants type))
  (define type-size (record-size variants))
  (define constructors (vector->immutable-vector
                        (for/vector #:length type-size
                          ([fields (record-values variants)]
                           [i (in-naturals)])
                          (define raw-constructor (λ vals (tuple-constructor i (list->immutable-vector vals))))
                          (procedure-reduce-arity raw-constructor (sequence-length fields)))))
  (define projectors (vector->immutable-vector
                      (for/vector #:length type-size
                        ([name (record-keywords variants)]
                         [fields (record-values variants)]
                         [i (in-naturals)])
                        (define size (sequence-length fields))
                        (λ (inst index)
                          (cond [(not (= i (tuple-accessor inst 0))) (raise-type-error 'projector-type-mismatch (keyword->string name) inst)]
                                [(>= index size) (raise-argument-error 'index-out-of-bounds "index too large" index)]
                                [else (immutable-vector-ref (tuple-accessor inst 1) index)])))))
  (define (enumerator inst) (tuple-accessor inst 0))
  (if (initialized-tuple-descriptor? descriptor)
      (initialized-algebraic-descriptor #:type type
                                        #:predicate predicate
                                        #:constructors constructors
                                        #:accessors projectors
                                        #:enumerator enumerator
                                        #:backing-tuple-descriptor descriptor)
      (uninitialized-algebraic-descriptor #:type type
                                          #:predicate predicate
                                          #:constructors constructors
                                          #:accessors projectors
                                          #:enumerator enumerator)))

(define (algebraic-type->tuple-type type)
  (tuple-type (algebraic-type-name type) '(index data)))


(define (default-algebraic-properties descriptor)
  (list (cons prop:custom-write (default-algebraic-custom-write descriptor))
        (cons prop:custom-print-quotable 'never)))

(define (default-algebraic-custom-write descriptor)
  (define type (algebraic-descriptor.type descriptor))
  (define accessors (algebraic-descriptor.accessors descriptor))
  (define enumerator (algebraic-descriptor.enumerator descriptor))
  (define name (algebraic-type-name type))
  (define prefix (string-append "(" (symbol->string name) ":"))
  (define printers
    (vector->immutable-vector
     (for/vector #:length (record-size (algebraic-type-variants type))
       ([name (record-keywords (algebraic-type-variants type))]
        [field-names (record-values (algebraic-type-variants type))]
        [accessor accessors])
       (λ (inst out)
         (write-string (keyword->string name) out)
         (for ([field-name field-names]
               [i (in-naturals)])
           (write-string " " out)
           (write field-name out)
           (write-string " " out)
           (write (accessor inst i) out))
         (void)))))
  (λ (self out mode)
    (write-string prefix out)
    ((vector-ref printers (enumerator self)) self out)
    (write-string ")" out)
    (void)))


(define (make-keyword-constructors descriptor)
  (define type (algebraic-descriptor.type descriptor))
  (define variants (algebraic-type-variants type))
  (vector->immutable-vector
   (for/vector #:length (record-size variants)
     ([name (record-keywords variants)]
      [field-seq (record-values variants)]
      [constructor (algebraic-descriptor.constructors descriptor)]
      [i (in-naturals)])
     (define raw-constructor (make-keyword-procedure (λ (kws kw-vals . rest) (constructor i kw-vals))))
     (define fields (sequence->list field-seq))
     (procedure-reduce-keyword-arity raw-constructor 0 fields fields))))
