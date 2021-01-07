#lang racket/base

(provide call-with-elle-reading-parameterization)


(define read-paren-with-tag (make-parameter #f))

(define (call-without-delimiter-tag-reading proc)
  (parameterize ([read-paren-with-tag #f]
                 [read-square-bracket-with-tag #f]
                 [read-curly-brace-with-tag #f])
    (proc)))

(define elle-readtable
  (make-readtable #f
                  #\( 'dispatch-macro
                  (case-lambda
                    [(ch in) (call-without-delimiter-tag-reading
                              (λ () (read/recursive in #\()))]
                    [(ch in src ln col pos) (call-without-delimiter-tag-reading
                                             (λ () (read-syntax/recursive src in #\()))])
                  #\( 'terminating-macro
                  (case-lambda
                    [(ch in) (if (read-paren-with-tag)
                                 (cons '#%parens (read/recursive in #\( #f))
                                 (read/recursive in #\( #f))]
                    [(ch in src ln col pos) (if (read-paren-with-tag)
                                                (cons '#%parens (read-syntax/recursive src in #\( #f))
                                                (read-syntax/recursive src in #\( #f))])))

(define (call-with-elle-reading-parameterization proc)
  (parameterize ([read-accept-dot #f]
                 [read-accept-infix-dot #f]
                 [read-accept-box #f]
                 [read-accept-graph #f]
                 [read-single-flonum #f]
                 [read-accept-quasiquote #f]
                 [read-accept-bar-quote #f]
                 [read-paren-with-tag #t]
                 [read-square-bracket-with-tag #t]
                 [read-curly-brace-with-tag #t]
                 [current-readtable elle-readtable])
    (proc)))
