(library (test-harness)
  (export print
          test-no-error
          test-error
          test-error-type
          test-error-message
          test-equal
          test-eqv
          test-eq
          summarize-tests)
  (import (rnrs))

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

  (define (condition-primary-type c)
    (let ([components (simple-conditions c)])
      (if (null? components)
          (record-type-name (record-rtd c))
          (record-type-name (record-rtd (car components))))))

  (define (print-condition c)
    (define (print-simple-condition c)
      (print "      " c))
    (print "    Condition" (condition-primary-type c))
    (for-each print-simple-condition (simple-conditions c))
    (print))

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

  (define (apply-if-applicable f . args)
    (cond
     [(procedure? f) (apply f args)]
     [else f]))

  (define no-expectation (cons '() '()))

  (define (print-failure exp verb actual expected)
    (let ([pact (if (condition? actual)
                    (condition-primary-type actual)
                    actual)])
      (if (eq? expected no-expectation)
          (print "FAIL" exp verb pact)
          (print "FAIL" exp verb pact "but expected" expected))))

  (define-syntax general-test
    (syntax-rules ()
      [(_ exp error-filter result-filter expected)
       (call/cc
        (lambda (k)
          (with-exception-handler
           (lambda (cond)
             (if (apply-if-applicable error-filter cond)
                 (test-passed)
                 (begin
                   (print-failure 'exp "raised" cond expected)
                   (print-condition cond)
                   (test-failed)))
             (k #f))
           (lambda ()
             (let ([actual exp])
             (if (apply-if-applicable result-filter actual)
                   (test-passed)
                   (begin
                     (print-failure 'exp "=>" actual expected)
                     (test-failed))))))))]))

  (define-syntax test-no-error
    (syntax-rules ()
      [(_ exp)
       (general-test exp #f #t no-expectation)]))

  (define-syntax test-error
    (syntax-rules ()
      [(_ exp)
       (general-test exp #t #f no-expectation)]))

  (define-syntax test-equal
    (syntax-rules ()
      [(_ expected exp)
       (general-test exp
                     #f
                     (lambda (actual) (equal? actual expected))
                     expected)]))
                     
  (define-syntax test-eqv
    (syntax-rules ()
      [(_ expected exp)
       (general-test exp
                     #f
                     (lambda (actual) (eqv? actual expected))
                     expected)]))
                     
  (define-syntax test-eq
    (syntax-rules ()
      [(_ expected exp)
       (general-test exp
                     #f
                     (lambda (actual) (eq? actual expected))
                     expected)]))
                     
  (define-syntax test-error-type
    (syntax-rules ()
      [(_ cond-type exp)
       (general-test exp
                     (lambda (cond)
                       (eq? (condition-primary-type cond) cond-type))
                     #f
                     cond-type)]))

  (define-syntax test-error-message
    (syntax-rules ()
      [(_ cond-msg exp)
       (general-test exp
                     (lambda (cond)
                       (string=? (condition-message cond) cond-msg))
                     #f
                     cond-msg)]))

  (define (summarize-tests)
    (print (pass-count) "passed")
    (unless (= 0 (fail-count))
            (print (fail-count) "failed")
            (exit 1)))

) ; end library
