#lang elle/base

{reprovide elle/base #:exposing-all}
{reprovide elle/prelude #:exposing-all}


#(module reader syntax/module-reader elle/main
   #:wrapper1 call-with-elle-reading-parameterization
   (require elle/private/reader))
