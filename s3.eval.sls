(library (s3.eval)
  (export eval)
  (import (rnrs)
          (s3.env))

  (define (self-evaluating? exp)
    (or (boolean? exp) (number? exp) (string? exp) (char? exp)))

  (define (evaluate-sequence exp* env)
    (if (pair? exp*)
        (if (pair? (cdr exp*))
            (begin
              (evaluate (car exp*) env)
              (evaluate-sequence (cdr exp*) env))
            (evaluate (car exp*) env))
        (error 'eval "empty begin" exp*)))

  (define (evaluate-list exps env)
    (if (pair? exps)
        (let ([arg1 (evaluate (car exps) env)])
          (cons arg1 (evaluate-list (cdr exps) env)))
        '()))

  (define (make-function args body env)
    (lambda values
      (evaluate-sequence body (extend-env args values env))))

  (define (invoke fn args)
    (if (procedure? fn)
        (apply fn args)
        (error 'eval "not a procedure" fn)))

  (define (evaluate exp env)
    (if (pair? exp)
        (case (car exp)
          [(quote) (cadr exp)]
          [(if) (if (evaluate (cadr exp) env)
                    (evaluate (caddr exp) env)
                    (unless (null? (cdddr exp))
                            (evaluate (cadddr exp) env)))]
          [(begin) (evaluate-sequence (cdr exp)
                                      env)]
          [(set!) (env-set-value! (cadr exp)
                                  (evaluate (caddr exp) env)
                                  env)]
          [(lambda) (make-function (cadr exp)
                                   (cddr exp)
                                   env)]
          [else (invoke (evaluate (car exp) env)
                        (evaluate-list (cdr exp) env))])
        (cond
         [(symbol? exp) (env-value exp env)]
         [(self-evaluating? exp) exp]
         [else (error 'eval "can't evaluate" exp)])))

  (define (compile exp env)
    ;; Nothing here yet.
    exp)
                                
  (define (eval exp env)
    (evaluate (compile exp env) env))


)
