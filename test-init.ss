#!/usr/bin/env scheme-script
(import (rnrs)
        (s3.init)
        (test-harness))

(test-no-error (initial-environment))

(summarize-tests)
