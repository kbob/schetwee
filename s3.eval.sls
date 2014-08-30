(library (s3.eval)
  (export eval)
  (import (rnrs)
          (s3.env))

  (define (self-evaluating? exp)
    (or (boolean? exp) (number? exp) (string? exp) (char? exp)))

  (define (eval-sequence exp* env)
    (if (pair? exp*)
        (if (pair? (cdr exp*))
            (begin
              (eval (car exp*) env)
              (eval-sequence (cdr exp*) env))
            (eval (car exp*) env))
        (error 'eval "empty begin" exp*)))

  (define (eval-list exps env)
    (if (pair? exps)
        (let ([arg1 (eval (car exps) env)])
          (cons arg1 (eval-list (cdr exps) env)))
        '()))

  (define (make-function args body env)
    (lambda values
      (eval-sequence body (extend-env args values env))))

  (define (invoke fn args)
    (if (procedure? fn)
        (apply fn args)
        (error 'eval "not a procedure" fn)))

  (define (eval exp env)
    (if (pair? exp)
        (case (car exp)
          [(quote) (cadr exp)]
          [(if) (if (eval (cadr exp) env)
                    (eval (caddr exp) env)
                    (unless (null? (cdddr exp))
                            (eval (cadddr exp) env)))]
          [(begin) (eval-sequence (cdr exp)
                                  env)]
          [(set!) (env-set-value! (cadr exp)
                                  (eval (caddr exp) env)
                                  env)]
          [(lambda) (make-function (cadr exp)
                                   (cddr exp)
                                   env)]
          [else (invoke (eval (car exp) env)
                        (eval-list (cdr exp) env))])
        (cond
         [(symbol? exp) (env-value exp env)]
         [(self-evaluating? exp) exp]
         [else (error 'eval "can't evaluate" exp)])))

)
