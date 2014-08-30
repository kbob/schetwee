(library (s3.env)
  (export push-env
          extend-env
          env-type
          env-value
          env-set-value!
          initial-environment)
  (import (rnrs)
          (rnrs mutable-pairs))

  (define *global-env* '())

  (define (define-global name value)
    (set! *global-env*
          (push-env name 'global value *global-env*)))

  (define (define-globals)
    (define-global 'eq? eq?)
    ; XXX Add more...
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

  (define (extend-env names values env)
    (cond
     [(or (null? names) (null? values))
      (unless (null? values)
              (error 'extend-env "too many values" values))
      (unless (null? names)
              (error 'extend-env "too few values" names))
      env]
     [else
      (extend-env (cdr names)
                  (cdr values)
                  (push-env (car names) (car values) env))]))

  (define (lookup name env caller)
    (cond
     [(null? env) (error caller "undefined" name)]
     [(eq? (caar env) name) (car env)]
     [else (lookup name (cdr env) caller)]))

  (define (env-type name env)
    (cadr (lookup name env 'env-type)))

  (define (env-value name env)
    (caddr (lookup name env 'env-type)))

  (define (env-set-value! name new-value env)
    (set-car! (cddr (lookup name env 'env-set-value!)) new-value))

)
