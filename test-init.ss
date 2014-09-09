#!/usr/bin/env scheme-script
(import (rnrs)
        (s3.init)
        (s3.env)
        (test-harness))

(test-no-error (initial-environment))
(test-eq (initial-environment) (initial-environment))
(test-eq 'global (env-type 'cons (initial-environment)))
(test-eq car (env-value 'car (initial-environment)))

(summarize-tests)
