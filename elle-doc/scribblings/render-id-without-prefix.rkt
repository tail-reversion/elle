#lang scribble/manual

@; From @soegaard2

@;{
 Here is a more complete solution.
 In order for @racket[rkt:id] to render as @racket[id],
 we need to import id without prefix. So I think one needs
 to put the code below in a separate module and then
 export all identifiers that begin with rkt:
}


@;{
 @(require (for-label racket))
 @(require (for-syntax racket/base scribble/manual syntax/parse racket/syntax scribble/racket))
 @(require (for-syntax doc-coverage racket/set))
 @(require scribble/racket)


 @(define-syntax (define-elements-for-prefixed-imports stx)
    (syntax-parse stx
      [(_ prefix modname) 
       (define all-exports ; make sure each identifer occurs once only
         (set->list (list->set (module->all-exported-names (syntax-e #'modname)))))      
       (define (format-sym    id) (format-id stx "~a"              id #:source stx))
       (define (format-pre:id id) (format-id stx "~a:~a" #'prefix  id #:source stx))
       (define (format-elm:id id) (format-id stx "~a:~a" #'elm1234 id #:source stx))      
       (with-syntax* ([(id ...)     (map format-sym    all-exports)]
                      [(pre:id ...) (map format-pre:id all-exports)]
                      [(elm:id ...) (map format-elm:id all-exports)])
         (syntax/loc stx
           (begin
             (begin
               (define        elm:id @racket[id])
               (define-syntax pre:id (make-element-id-transformer (λ _ #'elm:id))))
             ...)))]))


 @(define-elements-for-prefixed-imports rkt racket/base)

 @section{Foo}

 @racket[rkt:define]
}