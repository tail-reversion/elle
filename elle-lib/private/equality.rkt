#lang elle/private/prebase

{provide ≠? =?}


{require racket/base #:exposing equal? #:as =?}
{require elle/private/boolean #:exposing negate}

≠? = (negate =?)
