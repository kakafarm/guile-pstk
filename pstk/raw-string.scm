(define-module (pstk raw-string)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports))

(define (read-expected expected-string port)
  (with-input-from-port port
    (lambda ()
      (let loop ((expected-chars (string->list expected-string))
                 (c (read-char)))
        (cond
         ((eof-object? c)
          (null? expected-chars))
         ((null? expected-chars)
          (unread-char c)
          #t)
         ((char=? (car expected-chars)
                  c)
          (cond
           ((loop (cdr expected-chars)
                  (read-char))
            #t)
           (else
            (unread-char c)
            #f)))
         (else
          (unread-char c)
          #f))))))

(define (stack->string stack-of-chars)
  (list->string (reverse stack-of-chars)))

(eval-when (eval load compile expand)
  (define (raw-string-reader char port)
    (unless (read-expected "(" port)
      (error "Bad raw string"))

    (cond
     ((read-expected ")R#" port)
      "")
     (else
      (let loop ((c (read-char port))
                 (stack '()))
        (format #t "~s~%" stack)
        (cond
         ((eof-object? c)
          (error "Unexpected end of file" (stack->string stack)))
         ((read-expected ")R#" port)
          (stack->string (cons c stack)))
         (else
          (loop (read-char port)
                (cons c stack))))))))
  (read-hash-extend #\R raw-string-reader))
