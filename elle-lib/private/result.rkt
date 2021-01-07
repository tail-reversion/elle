#lang elle/private/prebase

{reprovide rebellion/base/result
           #:exposing
           result? result/c result-case
           success? success/c success success-value
           failure? failure/c failure failure-error}
