#lang elle/private/prebase

{reprovide rebellion/base/option
           #:exposing
           option?
           present? present present-value
           absent? absent
           option-case
           option-map option-flat-map option-filter
           option-get
           option/c present/c}
