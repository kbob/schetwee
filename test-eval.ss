#!/usr/bin/env scheme-script
(import (rnrs)
        (s3.eval)
        (s3.env)
        (test-harness))

(let* ([e0  '()]
       [e1  (extend-env '(n1 n2) '(v1 #f) e0)]
       [ei  (initial-environment)]
       [ei1 (extend-env '(n1 n2) '(v1 #f) ei)]
       [str "a string"])

  ;; atoms

  (test-eqv #t  (eval #t  e0))
  (test-eqv 42  (eval 42  e0))
  (test-eqv str (eval str e0))
  (test-eqv #\A (eval #\A e0))
  (test-eqv 'v1 (eval 'n1 e1))

  (test-error-type    '&syntax         (eval '#() e0))
  (test-error-message "can't evaluate" (eval '#() e0))

  (test-error-type    '&syntax         (eval '() e0))
  (test-error-message "can't evaluate" (eval '() e0))

  ;; quote

  (test-eqv   'sym (eval ''sym e0))
  (test-equal '(a) (eval ''(a) e0))

  ;; if

  (test-eqv 3    (eval '(if n1 3 4) e1))
  (test-eqv 4    (eval '(if n2 3 4) e1))
  (test-eqv 4    (eval '(if #f 3 4) e1))
  (test-eqv 3    (eval '(if #t 3)   e1))
  (test-no-error (eval '(if #f 3)   e1))

  ;; begin

  (test-eqv #f   (eval '(begin #f) e1))
  (test-eqv 'v1  (eval '(begin n1) e1))
  (test-eqv 'v1  (eval '(begin n2 n1) e1))

  (test-error-type    '&syntax      (eval '(begin) e1))
  (test-error-message "empty begin" (eval '(begin) e1))

  ;; set!

  (eval '(set! n2 'xx) e1)
  (test-eqv 'xx (env-value 'n2 e1))

  ;; invocation

  (test-eqv #f   (eval '(eq? 1 2) ei))
  (test-eqv #t   (eval '(eq? 3 3) ei))

  ;; procedures

  (test-no-error (eval '(lambda () 3) e0))
  (test-eqv 3 (eval '((lambda () 3)) e0))
  (test-eqv #t (eval '((lambda (x) (eq? x 4)) 4) ei))
  (test-eqv #t (eval '((lambda (a b) (eq? a b)) #\a #\a) ei))
  (test-eqv #t (eval '((lambda (a) (eq? n1 a)) 'v1) ei1))
  (test-eqv #f (eval '((lambda (a) (eq? n2 a)) 'v1) ei1))
  (test-eqv #f (eval '((lambda (a) ((lambda (b) (eq? a b)) 2)) 1) ei1))

  (test-error-type    '&assertion       (eval '(3) e0))
  (test-error-message "not a procedure" (eval '(3) e0))

  ) ; end let

(summarize-tests)
