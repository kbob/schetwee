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
        (raise (condition (make-syntax-violation '(begin) #f)
                          (make-who-condition 'begin)
                          (make-message-condition "empty begin")
                          (make-irritants-condition exp*)))))

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
        (raise (condition (make-assertion-violation)
                          (make-who-condition 'apply)
                          (make-message-condition "not a procedure")
                          (make-irritants-condition (list fn))))))

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
         [else (raise (condition (make-syntax-violation exp #f)
                                 (make-who-condition 'eval)
                                 (make-message-condition "can't evaluate")))])))

  (define (compile exp env)
    ;; Nothing here yet.
    exp)
                                
  (define (eval exp env)
    (evaluate (compile exp env) env))

) ; end library
