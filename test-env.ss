#!/usr/bin/env scheme-script
(import (rnrs)
        ;; (prefix (only (ikarus) pretty-print) ik.)
        (s3.env))

(let ([e1 (push-env 'my-name 'my-value (initial-environment))])
  (display e1) (newline)
  (env-set-value! 'my-name 'my-new-value e1)
  (display e1) (newline)
  (let ([e2 (push-env 'name2 'val2 e1)])
    (display e2) (newline)
    (display e1) (newline)))
