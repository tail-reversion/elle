#lang elle/private/prebase

{reprovide elle/private/prebase #:exposing-all}
{reprovide elle/private/boolean #:exposing-all}
{reprovide elle/private/char #:exposing-all}
{reprovide elle/private/equality #:exposing-all}
{reprovide elle/private/keyword #:exposing-all}
{reprovide elle/private/number #:exposing-all}
{reprovide elle/private/option #:exposing-all}
{reprovide elle/private/ordering #:exposing-all}
{reprovide elle/private/procedure #:exposing-all}
{reprovide elle/private/result #:exposing-all}
{reprovide elle/private/symbol #:exposing-all}
{reprovide elle/private/text #:exposing-all}


#(module reader syntax/module-reader elle/base
   #:wrapper1 call-with-elle-reading-parameterization
   (require elle/private/reader))
