/*
Para melhor leitura, deixe um espaço em branco de duas linhas entre as funções

*/

(define (interpret program)
  

  (define environment '())
  

  (define (get-value name env)
    (cond
      ((assq name env) => cdr) ; Procura a variável no ambiente local
      ((null? env) (error "Variável não definida:" name)) ; Se não encontrar, gera um erro
      (else (get-value name (cdr env) ))
    )
  ) ; Procura no ambiente pai
  

  (define (set-value! name value env)
    (if (assq name env)
        (set-cdr! (assq name env) value)
        (set! environment (cons (cons name value) environment))
    )
  )


  (define (evaluate-exp exp env)
    (cond
      ((number? exp) exp) ; Números são valores diretos
      ((string? exp) exp) ; Strings são valores diretos
      ((symbol? exp) (get-value exp env)) ; Variáveis
      ((eq? (car exp) 'print) (display (evaluate-exp (cadr exp) env)) (newline)) ; Função print
      ((eq? (car exp) 'function) ; Definição de função
       (let ((func-name (cadr exp))
             (params (caddr exp))
             (local-vars (cadddr exp))
             (body (cddddr exp)))
         (set-value! func-name
                     (lambda args
                       (let ((new-env (append (map cons params args) (map (lambda (v) (cons v 0)) local-vars))))
                         (evaluate-body body new-env))))
         func-name))
      ((eq? (car exp) 'begin) ; Sequência de comandos
       (define (eval-sequence seq env)
         (if (null? (cdr seq))
             (evaluate-exp (car seq) env)
             (eval-sequence (cdr seq) env)))
       (eval-sequence (cdr exp) env))
      ((eq? (car exp) 'return) ; Retorno de valor
       (evaluate-exp (cadr exp) env))
      ((eq? (car exp) 'if) ; Condicional
       (if (evaluate-exp (cadr exp) env)
           (evaluate-exp (caddr exp) env)
           (evaluate-exp (cadddr exp) env)))
      ((eq? (car exp) 'set!) ; Atribuição
       (let ((var (cadr exp))
             (value (evaluate-exp (caddr exp) env)))
         (set-value! var value env)
         '()))
      (else
       (let ((func (evaluate-exp (car exp) env))
             (args (map (lambda (arg) (evaluate-exp arg env)) (cdr exp))))
         (apply func args)))))
  
  (define (evaluate-body body env)
    (if (null? (cdr body))
        (evaluate-exp (car body) env)
        (begin
          (evaluate-exp (car body) env)
          (evaluate-body (cdr body) env))))
  
  (define (interpret-program program)
    (let ((main-func (cadr program)))
      (evaluate-body (cddr main-func) environment)))
  
  (interpret-program program)
)

;; Exemplo de uso:

(define programa-exemplo
  '(function main < >
     vars a b c
     begin
       (set! a 10)
       (set! b 15.5)
       (set! a b)
       (set! b (+ 1 5.7))
       (set! c (- 2.67 a))
       (set! a (* b a))
       (set! b (+ c -6))
       (if (> a -10.5)
           (set! a (f < b 5.5 >))
           (set! a b))
       (print a)
       (print b)
       (print c)
       (return a)
     end))

(interpret programa-exemplo)