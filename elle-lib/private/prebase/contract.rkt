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
         any
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
             #:attr (names 1) (syntax->list #'({~@ 'kw (contract-name ctc.c)} ...))
             #:attr (contracts 1) (datum ({~@ kw ctc.c} ...))])

  (define-splicing-syntax-class mandatory-arguments-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq margs:positional-argument-contracts mkwargs:keyword-argument-contracts}
             #:attr (names 1) (datum (margs.names ... mkwargs.names ...))
             #:attr (contracts 1) (datum (margs.contracts ... mkwargs.contracts ...))])

  (define-syntax-class optional-arguments-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    #:literals (#%brackets)
    [pattern (#%brackets oargs:positional-argument-contracts okwargs:keyword-argument-contracts)
             #:attr (names 1) (datum (oargs.names ... okwargs.names ...))
             #:attr (contracts 1) (datum (oargs.contracts ... okwargs.contracts ...))])
  
  (define-syntax-class result-contract
    #:description "procedure result contract"
    #:attributes (name normalized)
    #:literals (#%parens values any)
    [pattern any
             #:attr name #''any
             #:attr normalized #'any]
    [pattern ctc
             #:declare ctc (expr/c #'contract?)
             #:attr name #'(contract-name ctc.c)
             #:attr normalized #'ctc.c]
    [pattern (#%parens values ctc ...)
             #:declare ctc (expr/c #'contract?)
             #:attr name #'(list 'values (contract-name ctc.c) ...)
             #:attr normalized #'(values ctc.c ...)])
  }


(define-syntax-parser λ/c
  #:track-literals
  #:literals (→)
  [(_ margs:mandatory-arguments-contracts oargs:optional-arguments-contracts rest-ctc {~literal ...} → result:result-contract)
   #:declare rest-ctc (expr/c #'contract?)
   #:with name #'(list 'λ/c margs.names ... (list oargs.names ...) (contract-name rest-ctc.c) {... '...} '→ result.name)
   #:with contract #'(->* (margs.contracts ...) (oargs.contracts ...) #:rest (listof rest-ctc.c) result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts oargs:optional-arguments-contracts → result:result-contract)
   #:with name #'(list 'λ/c margs.names ... (list oargs.names ...) '→ result.name)
   #:with contract #'(->* (margs.contracts ...) (oargs.contracts ...) result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts rest-ctc {~literal ...+} → result:result-contract)
   #:declare rest-ctc (expr/c #'contract?)
   #:with name #'(list 'λ/c margs.names ... (contract-name rest-ctc.c) '...+ '→ result.name)
   #:with contract #'(-> margs.contracts ... rest-ctc.c rest-ctc.c {... ...} result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts rest-ctc {~literal ...} → result:result-contract)
   #:declare rest-ctc (expr/c #'contract?)
   #:with name #'(list 'λ/c margs.names ... (contract-name rest-ctc.c) {... '...} '→ result.name)
   #:with contract #'(-> margs.contracts ... rest-ctc.c {... ...} result.normalized)
   #'(rename-contract contract name)]
  [(_ margs:mandatory-arguments-contracts → result:result-contract)
   #:with name #'(list 'λ/c margs.names ... '→ result.name)
   #:with contract #'(-> margs.contracts ... result.normalized)
   #'(rename-contract contract name)])



(define-syntax (Π/c stx)
  (raise-syntax-error #f "not for use as an expression" stx))

{begin-for-syntax
  (define-splicing-syntax-class dependent-contract
    #:description ""
    #:attributes (name contract)
    #:literals (: #%braces Π/c ⇒)
    [pattern {~seq var:id : (#%braces Π/c arg:id ...+ ⇒ ctc-expr)}
             #:declare ctc-expr (expr/c #'contract?)
             #:attr name #'('var ': (list 'Π/c 'arg ... '⇒ 'ctc-expr))
             #:attr contract #'(var (arg ...) ctc-expr.c)]
    [pattern {~seq var:id : ctc}
             #:declare ctc (expr/c #'contract?)
             #:attr name #'('var ': (contract-name ctc.c))
             #:attr contract #'(var ctc.c)])
  
  (define-splicing-syntax-class dependent-positional-argument-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq arg:dependent-contract ...}
             #:attr (names 1) (datum ({~@ . arg.name} ...))
             #:attr (contracts 1) (datum (arg.contract ...))])

  (define-splicing-syntax-class dependent-keyword-argument-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq {~seq kw:keyword arg:dependent-contract} ...}
             #:attr (names 1) (datum ({~@ kw {~@ . arg.name}} ...))
             #:attr (contracts 1) (datum ({~@ kw arg.contract} ...))])

  (define-splicing-syntax-class mandatory-dependent-argument-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    [pattern {~seq args:dependent-positional-argument-contracts kwargs:dependent-keyword-argument-contracts}
             #:attr (names 1) (datum (args.names ... kwargs.names ...))
             #:attr (contracts 1) (datum (args.contracts ... kwargs.contracts ...))])

  (define-syntax-class optional-dependent-argument-contracts
    #:description ""
    #:attributes ([names 1] [contracts 1])
    #:literals (#%brackets)
    [pattern (#%brackets args:dependent-positional-argument-contracts kwargs:dependent-keyword-argument-contracts)
             #:attr (names 1) (datum (args.names ... kwargs.names ...))
             #:attr (contracts 1) (datum (args.contracts ... kwargs.contracts ...))])

  (define-splicing-syntax-class dependent-rest-contract
    #:description ""
    #:attributes (name contract one-or-more?)
    [pattern {~seq arg:dependent-contract {~literal ...}}
             #:attr name #'(arg.name {... '...})
             #:attr contract #'arg.contract
             #:attr one-or-more? #f]
    [pattern {~seq arg:dependent-contract {~literal ...+}}
             #:attr name #'(arg.name '...+)
             #:attr contract #'arg.contract
             #:attr one-or-more? #t])

  (define-splicing-syntax-class dependent-result-contract
    #:description ""
    #:attributes (name contract)
    #:literals (any values #%parens)
    [pattern any
             #:attr name #''any
             #:attr contract #'any]
    [pattern (#%parens values dep-ctc:dependent-contract ...)
             #:attr name #'(list 'values {~@ . dep-ctc.name} ...)
             #:attr contract #'(values dep-ctc.contract ...)]
    [pattern dep-ctc:dependent-contract
             #:attr name (attribute dep-ctc.name)
             #:attr contract (attribute dep-ctc.contract)])
  }

(define-syntax-parser λΠ/c
  #:track-literals
  #:literals (→)
  [(_ {~optional {~and chap #:chaperone}} margs:mandatory-dependent-argument-contracts oargs:optional-dependent-argument-contracts → result:dependent-result-contract)
   #:with name #'(list 'λΠ/c {~? 'chap} margs.names ... (list oargs.names ...) '→ {~@ . result.name})
   #:with contract #'(->i {~? chap} (margs.contracts ...) (oargs.contracts ...) result.contract)
   #'(rename-contract contract name)]
  [(_ {~optional {~and chap #:chaperone}} margs:mandatory-dependent-argument-contracts rest:dependent-rest-contract → result:dependent-result-contract)
   #:with name #'(list 'λΠ/c {~? 'chap} margs.names ... {~@ . rest.name} '→ {~@ . result.name})
   #:with maybe-one (if (attribute rest.one-or-more?) #'(rest.contract) #'())
   #:with contract #'(->i {~? chap} (margs.contracts ... {~@ . maybe-one} #:rest (listof rest.contract) result.contract))
   #'(rename-contract contract name)]
  [(_ {~optional {~and chap #:chaperone}} margs:mandatory-dependent-argument-contracts → result:dependent-result-contract)
   #:with name #'(list 'λΠ/c {~? 'chap} margs.names ... '→ {~@ . result.name})
   #:with contract #'(->i {~? chap} (margs.contracts ...) result.contract)
   #'(rename-contract contract name)])
