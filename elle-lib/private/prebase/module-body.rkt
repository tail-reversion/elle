#lang racket/base

(provide (rename-out [@%module-begin #%module-begin]))

(require (for-syntax racket/base racket/string syntax/datum syntax/id-table)
         racket/contract
         syntax/parse/define
         elle/private/prebase/preprebase
         (only-in elle/private/prebase/match values-pattern)
         (only-in elle/private/prebase/let-lam procedure-header)
         (only-in racket/match match-define-values))


(module+ secret-provide
  (provide (for-syntax lookup-contract)))


(define-syntax-parser @%module-begin
  #:track-literals
  [(_ form ...) #'(#%plain-module-begin
                   (module configure-runtime racket/base
                     (require elle/private/reader)
                     (current-read-interaction (λ (src in)
                                                 (call-with-elle-reading-parameterization
                                                  (λ () (read-syntax src in)))))
                     (print-boolean-long-form #true))
                   (parse-module-body form ...))])


(define-for-syntax contract-mappings (make-free-id-table))

(define-for-syntax (register-contract! id ctc)
  (free-id-table-set! contract-mappings id ctc))

(define-for-syntax (lookup-contract id)
  (free-id-table-ref contract-mappings id #f))


(define-syntax-parser defvals
  #:track-literals
  [(_ binding:values-pattern e:expr)
   #'(match-define-values (binding.normalized ...) e)])

(define-syntax-parser defun
  #:track-literals
  [(_ header:procedure-header body:expr)
   #'(define header.name ({~@ . header.curried-λ-heads} ... body))])


(define-syntax-parser parse-module-body
  #:track-literals
  #:literals (: = ⇒ #%braces)
  [(_ {~seq name:id : ctc} ...+ binding:values-pattern = e:expr form ...)
   #:declare ctc (expr/c #'contract?)
   (for ([name (in-list (datum (name ...)))]
         [ctc (in-list (datum (ctc.c ...)))])
     (register-contract! name ctc))
   #'(begin (defvals binding e)
            (parse-module-body form ...))]
  [(_ binding:values-pattern = e:expr form ...)
   #'(begin (defvals binding e)
            (parse-module-body form ...))]
  [(_ name:id : ctc header:procedure-header ⇒ e:expr form ...)
   #:declare ctc (expr/c #'contract?)
   (register-contract! #'name #'ctc.c)
   #'(begin (defun header e)
            (parse-module-body form ...))]
  [(_ header:procedure-header ⇒ e:expr form ...)
   #'(begin (defun header e)
            (parse-module-body form ...))]
  [(_ (#%braces stx ...) form ...)
   #'(begin (#%braces stx ...)
            (parse-module-body form ...))]
  [(_ (f stx ...) form ...)
   #'(begin (f stx ...)
            (parse-module-body form ...))]
  [(_) #'(begin)])



#;(begin-for-syntax
    (let ([current-handler (error-display-handler)])
      (error-display-handler
       (λ (s v)
         (current-handler (string-replace s "bad syntax" "macro identifier in expression position, possibly in parens")
                          v)))))
