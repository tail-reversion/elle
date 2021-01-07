#lang elle/base

{reprovide racket/base
           #:exposing
           abs
           max min
           gcd lcm
           round floor ceiling truncate
           sqrt
           expt exp log
           sin cos tan
           asin acos atan
           make-rectangular make-polar
           real-part imag-part
           magnitude angle}

{reprovide racket/math
           #:exposing
           sinh cosh tanh conjugate
           pi
           degrees->radians #:as degrees→radians
           radians->degrees #:as radians→degrees
           sgn}
