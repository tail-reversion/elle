#lang info

(define collection "elle")

(define deps '("base" "elle-lib" "scribble-lib"))

(define build-deps '("racket-doc" "rebellion"))

(define scribblings '(("scribblings/main.scrbl" (multi-page) (language) "Elle")))
