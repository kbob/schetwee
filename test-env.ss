#!/usr/bin/env scheme-script
(import (rnrs)
        (s3.env)
        (s3.init)
        (test-harness))

(let* ([e0 (initial-environment)]
       [e1 (push-env 'n1 'v1 e0)]
       [e2 (push-env 'n2 't1 'v2 e1)]
       [e3 (extend-env '(n3 n4) '(v3 v4) e1)])

  (test-error-type '&assertion
                   (extend-env '(n5) '(v5 v6) e0))
  (test-error-type '&assertion
                   (extend-env '(n5 n6) '(v5) e0))
  (test-error-message "incorrect number of arguments"
                      (extend-env '(n5) '(v5 v6) e0))
  (test-error-message "incorrect number of arguments"
                      (extend-env '(n5 n6) '(v5) e0))

  (test-eqv 'v1      (env-value 'n1 e1))
  (test-eqv 'lexical (env-type  'n1 e1))

  (test-error-type '&undefined (env-value 'n2 e1))
  (test-error-type '&undefined (env-type 'n2 e1))
  (test-error-message "undefined" (env-value 'n2 e1))
  (test-error-message "undefined" (env-type 'n2 e1))

  (test-eq 'v1      (env-value 'n1 e2))
  (test-eq 'lexical (env-type  'n1 e2))
  (test-eq 'v2      (env-value 'n2 e2))
  (test-eq 't1      (env-type  'n2 e2))

  (test-eq 'v1      (env-value 'n1 e3))
  (test-eq 'v3      (env-value 'n3 e3))
  (test-eq 'v4      (env-value 'n4 e3))

  (test-error-type '&undefined (env-value 'n2 e3))
  (test-error-type '&undefined (env-type 'n2 e3))
  (test-error-message "undefined" (env-value 'n2 e3))
  (test-error-message "undefined" (env-type 'n2 e3))

  (env-set-value! 'n1 'xx e1)
  (test-eq 'xx      (env-value 'n1 e1))
  (test-eq 'lexical (env-type  'n1 e1))
  (test-eq 'xx      (env-value 'n1 e2))
  (test-eq 'xx      (env-value 'n1 e3))

)

(summarize-tests)

