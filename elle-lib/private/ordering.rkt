#lang elle/private/prebase

{require racket/contract #:exposing -> any/c}

; ordered? : {-> any/c boolean?}
; <? : {-> ordered? ordered? boolean?}
; >? : {-> ordered? ordered? boolean?}
{provide gen:ordered ordered?
         <? >?}

{require racket/generic #:exposing-all}
{require racket/base #:exposing define #:as def}
{require rebellion/base/comparator #:exposing make-comparator compare comparison? lesser greater equivalent}
{require elle/private/equality #:exposing =?}
{require elle/private/boolean #:exposing boolean?}
{require elle/private/number #:exposing number? number<? number>?}
{require elle/private/char #:exposing char? char<? char>?}
{require elle/private/symbol #:exposing symbol? symbol<?}
{require elle/private/keyword #:exposing keyword? keyword<?}
{require elle/private/text #:exposing text? text<? text>?}
{require elle/private/bytes #:exposing bytes? bytes<? bytes>?}


#(define-generics ordered
   [<? ordered other]
   [>? ordered other]
   #:fallbacks [(define/generic gen:<? <?)
                (def (>? v1 v2) (gen:<? v2 v1))]
   #:fast-defaults ([number? (def <? number<?)
                             (def >? number>?)]
                    [char? (def <? char<?)
                           (def >? char>?)]
                    [symbol? (def <? symbol<?)]
                    [keyword? (def <? keyword<?)]
                    [text? (def <? text<?)
                           (def >? text>?)]
                    [bytes? (def <? bytes<?)
                            (def >? bytes>?)]))

any<=> = (make-comparator
          {λ v1 v2 ⇒ {cond (<? v1 v2) ⇒ lesser
                           (=? v1 v2) ⇒ equivalent
                           #:else greater}}
          #:name 'any<=>)

(<=> v1 v2) ⇒ (compare any<=> v1 v2)
