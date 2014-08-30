(library (test-harness)
  (export print
          test-runs
          test-equal
          test-eqv
          test-eq
          test-error
          test-error-message
          summarize-tests)
  (import (rnrs))

  ;; XXX have test catch exceptions, print them, and count a failure.
  ;;     (How do you print an exception?)
  ;; XXX refactor test-eq* to use common definition.
  ;; XXX allow definition of test suites and subsuites.
  ;;
  ;;   (test-suite my-module
  ;;     (test-eq ...)
  ;;     (test-error ...)
  ;;     (test-suite my-function
  ;;       more tests...)
  ;;     (test-suite my-other-function
  ;;       other tests...))

  (define (print . x)
    (let next ([l x])
      (cond
       [(pair? l)
        (display (car l))
        (display " ")
        (next (cdr l))]
       [(null? l) (newline)]
       [else
        (display l)
        (newline)])))

  (define pass-count
    (let ([*pass-count* 0])
      (case-lambda
       [() *pass-count*]
       [(inc) (set! *pass-count* (+ *pass-count* 1))])))

  (define (test-passed)
    (pass-count 'inc))

  (define fail-count
    (let ([*fail-count* 0])
      (case-lambda
       [() *fail-count*]
       [(inc) (set! *fail-count* (+ *fail-count* 1))])))

  (define (test-failed)
    (fail-count 'inc))

  (define-syntax test-runs
    (syntax-rules ()
      [(_ exp)
       (begin
         exp
         (test-passed))]))

  (define-syntax test-equal
    (syntax-rules ()
      [(_ expected exp)
       (let ([actual exp])
         (if (equal? expected actual)
             (test-passed)
             (begin
               (print "FAIL" 'exp "=>" actual "; expected" expected)
               (test-failed))))]))

  (define-syntax test-eqv
    (syntax-rules ()
      [(_ expected exp)
       (let ([actual exp])
         (if (eqv? expected actual)
             (test-passed)
             (begin
               (print "FAIL" 'exp "=>" actual "; expected" expected)
               (test-failed))))]))

  (define-syntax test-eq
    (syntax-rules ()
      [(_ expected exp)
       (let ([actual exp])
         (if (eq? expected actual)
             (test-passed)
             (begin
               (print "FAIL" 'exp "=>" actual "; expected" expected)
               (test-failed))))]))

  (define-syntax test-error
    (syntax-rules ()
      [(_ exp)
       (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (exc)
             (test-passed)
             (k #f))
           (lambda ()
             (let ([actual exp])
               (print "FAIL" 'exp "=>" actual "; expected error")
               (test-failed))))))]))

  (define-syntax test-error-message
    (syntax-rules ()
      [(_ msg exp)
       (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (exc)
             (let ([actual-msg (condition-message exc)])
               (if (equal? msg actual-msg)
                   (test-passed)
                   (begin
                     (print "FAIL" 'exp "raised" actual-msg "; expected" msg)
                     (test-failed))))
             (k #f))
           (lambda ()
             (let ([actual exp])
               (print "FAIL" exp "=>" actual "; expected error")
               (test-failed))))))]))

  (define (summarize-tests)
    (print (pass-count) "passed")
    (unless (= 0 (fail-count))
            (print (fail-count) "failed")
            (exit 1)))

) ; end library
