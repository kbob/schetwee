(library (s3.env)
  (export push-env env-type env-value env-set-value! initial-environment)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define *global-env* '())

  (define (define-global name value)
    (set! *global-env*
          (push-env name 'global value *global-env*)))

  (define (define-globals)
    (define-global 'eq? eq?)
    ; Add more...
    )

  (define (initial-environment)
    (if (null? *global-env*)
        (define-globals))
    *global-env*)

  (define push-env
    (case-lambda
     [(name value env)
      (push-env name 'lexical value env)]
     [(name type value env)
      (cons (list name type value) env)]))

    (define (env-type name env)
      (cond
       [(null? name) (error "undefined" name)]
       [(eqv? name (caar env)) (cadar env)]
       [else (env-value name (cdr env))]))

    (define (env-value name env)
      (cond
       [(null? name) (error "undefined" name)]
       [(eqv? name (caar env)) (caddar env)]
       [else (env-value name (cdr env))]))

    (define (env-set-value! name new-value env)
      (cond
       [(null? name) (error "undefined" name)]
       [(eqv? name (caar env)) (set-car! (cddar env) new-value)]
       [else (env-value name (cdr env))]))
      
)
