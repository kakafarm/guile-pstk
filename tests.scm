(import (srfi srfi-64)

        (ice-9 rdelim)
        (ice-9 textual-ports)

        (pstk))

(define-syntax-rule (test-read-expected input-string expected-string after-expected-string)
  (let* ((port (open-input-string input-string))
         (expected? (read-expected expected-string port)))

    (test-equal after-expected-string
      (get-string-all port))))

(test-begin "read-expected")

(test-read-expected "" "abc" "")
(test-read-expected "a" "abc" "a")
(test-read-expected "b" "abc" "b")

(test-read-expected "ab" "abc" "ab")
(test-read-expected "ba" "abc" "ba")
(test-read-expected "ac" "abc" "ac")

(test-read-expected "abc" "abc" "")
(test-read-expected "bbc" "abc" "bbc")
(test-read-expected "abd" "abc" "abd")
(test-read-expected "bbc" "abc" "bbc")
(test-read-expected "adc" "abc" "adc")
(test-read-expected "abcd" "abc" "d")
(test-read-expected "bbcd" "abc" "bbcd")
(test-read-expected "abdd" "abc" "abdd")
(test-read-expected "bbcd" "abc" "bbcd")
(test-read-expected "adcd" "abc" "adcd")

(test-end "read-expected")
