; input do buffer do teclado
(define (parse input)
  (define (skip-whitespace str)
    (if (string-null? str)
        ""
        (let ((char (string-ref str 0)))
          (if (char-whitespace? char)
              (skip-whitespace (substring str 1))
              str))))

; input de strings
  (define (parse-name str)
    (let ((name ""))
      (let loop ((str (skip-whitespace str)))
        (if (string-null? str)
            (values name "")
            (let ((char (string-ref str 0)))
              (if (or (char-alphabetic? char) (char=? char '-'))
                  (loop (substring str 1))
                  (values name str)))))))

; input de valores numericos
  (define (parse-numeric-value str)
    (let ((name "") (number ""))
      (let loop ((str (skip-whitespace str)))
        (if (string-null? str)
            (values name number "")
            (let ((char (string-ref str 0)))
              (if (or (char-numeric? char) (char-alphabetic? char))
                  (loop (substring str 1))
                  (values name number str)))))))

; chamada de funcao
  (define (parse-func-call str)
    (let-values (((name rest) (parse-name str)))
      (if (string=? rest "")
          (values name "")
          (let-values (((args rest) (parse-numeric-values (substring rest 1))))
            (values name args)))))

)