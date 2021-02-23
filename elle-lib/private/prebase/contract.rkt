#lang racket/base

(require racket/contract)

(provide flat-contract-with-explanation
         flat-named-contract
         contract?
         chaperone-contract?
         impersonator-contract?
         flat-contract?
         contract-name
         has-contract?
         has-blame?
         blame?
         contract-late-neg-projection
         contract-projection
         contract-val-first-projection
         recursive-contract
         rename-contract
         any/c
         none/c
         λ/c
         (contract-out
          [if/c {λ/c {λ/c any/c → boolean?} contract? contract? → contract?}]
          [rename @%value-contract value-contract {λ/c any/c → (option/c contract?)}]
          [rename @%value-blame value-blame {λ/c any/c → (option/c blame?)}])
         (rename-out
          [or/c ∪/c]
          [and/c ∩/c]
          [not/c ¬/c]))

(require (for-syntax racket/base syntax/datum)
         syntax/parse/define
         rebellion/base/option
         elle/private/prebase/preprebase)


(define (@%value-contract v)
  (let ([maybe-contract (value-contract v)])
    (if maybe-contract
        (present maybe-contract)
        absent)))

(define (@%value-blame v)
  (let ([maybe-blame (value-blame v)])
    (if maybe-blame
        (present maybe-blame)
        absent)))


(struct kwarg-name (kw name)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write (kwarg-name-kw self) port)
     (write-char #\space port)
     (write (kwarg-name-name self) port))])

(struct rest-name (name one-or-more?)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write (rest-name-name self) port)
     (write-char #\space port)
     (write (if (rest-name-one-or-more? self) '...+ '...) port))])

(struct procedure-contract-name (marg-names oarg-names rest-name result-name)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write-string "{λ/c" port)
     (let ([marg-names (procedure-contract-name-marg-names self)])
       (unless (null? marg-names)
         (for-each (λ (e)
                     (write-char #\space port)
                     (write e port))
                   marg-names)))
     (let ([oarg-names (procedure-contract-name-oarg-names self)])
       (unless (null? oarg-names)
         (write-string " [" port)
         (write (car oarg-names) port)
         (for-each (λ (e)
                     (write-char #\space port)
                     (write e port))
                   (cdr oarg-names))
         (write-char #\] port)))
     (let ([rest-name (procedure-contract-name-rest-name self)])
       (when rest-name
         (write-char #\space port)
         (write rest-name port)))
     (write-string " → " port)
     (write (procedure-contract-name-result-name self) port)
     (write-char #\} port))])

(define (make-unconstrained-domain-procedure-contract-name result-name)
  (procedure-contract-name '(#:unconstrained) '() #f result-name))


{begin-for-syntax
  (define-splicing-syntax-class positional-argument-contracts
    #:description "procedure positional argument contracts"
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq ctc ...}
             #:declare ctc (expr/c #'contract?)
             #:attr (names 1) (syntax->list #'((contract-name ctc.c) ...))
             #:attr (contracts 1) (datum (ctc.c ...))])

  (define-splicing-syntax-class keyword-argument-contracts
    #:description "procedure keyword argument contracts"
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq {~seq kw:keyword ctc} ...}
             #:declare ctc (expr/c #'contract?)
             #:attr (names 1) (syntax->list #'((kwarg-name 'kw (contract-name ctc.c)) ...))
             #:attr (contracts 1) (datum ({~@ kw ctc.c} ...))])

  (define-splicing-syntax-class mandatory-arguments-contracts
    #:description "procedure mandatory argument contracts"
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq margs:positional-argument-contracts mkwargs:keyword-argument-contracts}
             #:attr (names 1) (datum (margs.names ... mkwargs.names ...))
             #:attr (contracts 1) (datum (margs.contracts ... mkwargs.contracts ...))])

  (define-syntax-class optional-arguments-contracts
    #:description "procedure optional argument contracts"
    #:attributes ([names 1] [contracts 1])
    #:literals (#%brackets)
    [pattern (#%brackets oargs:positional-argument-contracts okwargs:keyword-argument-contracts)
             #:attr (names 1) (datum (oargs.names ... okwargs.names ...))
             #:attr (contracts 1) (datum (oargs.contracts ... okwargs.contracts ...))])

  (define-splicing-syntax-class rest-contract
    #:description "rest argument contract"
    #:attributes (name contract one-or-more?)
    [pattern {~seq rest {~literal ...}}
             #:declare rest (expr/c #'contract?)
             #:attr name #'(rest-name (contract-name rest.c) #f)
             #:attr contract #'rest.c
             #:attr one-or-more? #f]
    [pattern {~seq rest {~literal ...+}}
             #:declare rest (expr/c #'contract?)
             #:attr name #'(rest-name (contract-name rest.c) #t)
             #:attr contract #'rest.c
             #:attr one-or-more? #t])

  (define-splicing-syntax-class precondition
    #:description "procedure precondition"
    #:attributes (pre-name pre-expr)
    #:literals (#%brackets)
    [pattern {~seq #:pre (#%brackets pre-name:str pre-test fail-msg:str)}
             #:declare pre-test (expr/c #'boolean?)
             #:attr pre-expr #'(if pre-test.c #t  fail-msg)]
    [pattern {~seq #:pre (#%brackets pre-test fail-msg:str)}
             #:declare pre-test (expr/c #'boolean?)
             #:attr pre-name #''pre-test
             #:attr pre-expr #'(if pre-test.c #t fail-msg)])

  (define-splicing-syntax-class postcondition
    #:description "procedure postcondition"
    #:attributes (post-name post-expr)
    #:literals (#%brackets)
    [pattern {~seq #:post (#%brackets post-name:str post-test fail-msg:str)}
             #:declare post-test (expr/c #'boolean?)
             #:attr post-expr #'(if post-test.c #t fail-msg)]
    [pattern {~seq #:post (#%brackets post-test fail-msg:str)}
             #:declare post-test (expr/c #'boolean?)
             #:attr post-name #''post-test
             #:attr post-expr #'(if post-test.c #t fail-msg)])
  
  (define-syntax-class result-contract
    #:description "procedure result contract"
    #:attributes (name normalized)
    [pattern ctc
             #:declare ctc (expr/c #'contract?)
             #:attr name #'(contract-name ctc.c)
             #:attr normalized #'ctc.c])
  }


(define-syntax-parser λ/c
  #:track-literals
  #:literals (→)
  [(_ #:unconstrained → ctc)
   #:declare ctc (expr/c #'contract?)
   #:with name #'(make-unconstrained-domain-procedure-contract-name (contract-name ctc.c))
   #:with contract #'(unconstrained-domain-> ctc.c)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts oargs:optional-arguments-contracts rest:rest-contract → result:result-contract)
   #:with name #'(procedure-contract-name (list margs.names ...) (list oargs.names ...) rest.name result.name)
   #:with contract #`(->* (margs.contracts ...) (oargs.contracts ...) #:rest #,(if (attribute rest.one-or-more?)
                                                                                   #'(cons/c rest.contract (listof rest.contract))
                                                                                   #'(listof rest.contract))
                          result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts oargs:optional-arguments-contracts → result:result-contract)
   #:with name #'(procedure-contract-name (list margs.names ...) (list oargs.names ...) #f result.name)
   #:with contract #'(->* (margs.contracts ...) (oargs.contracts ...) result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts rest:rest-contract → result:result-contract)
   #:with name #'(procedure-contract-name (list margs.names ...) (list) rest.name result.name)
   #:with contract #`(-> margs.contracts ... #,@(if (attribute rest.one-or-more?)
                                                    #'(rest.contract rest.contract)
                                                    #'(rest.contract))
                         {... ...}
                         result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts → result:result-contract)
   #:with name #'(procedure-contract-name (list margs.names ...) (list) #f result.name)
   #:with contract #'(-> margs.contracts ... result.normalized)
   #'(rename-contract contract name)])
