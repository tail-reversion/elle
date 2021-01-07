#lang info

(define collection "elle")

(define deps '("base" "elle-lib" "scribble-lib"))

(define build-deps '("racket-doc"))

(define scribblings '(("scribblings/main.scrbl" (multi-page) (language) "Elle")))
