#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastian Tamayo - 2110331


;<exp-arit> :: = num-type  <num>
;           :: = symb-type <symbol>
;           :: = arit-type (<exp-arit> <operando> <exp-arit>)

;<operando> ::= <suma> | <resta> | <multiplicacion> | <division>


; <ambiente> ::= empty
;            ::= (extend-env <symbol> <valor> <ambiente>) 


;; ------------------------------------------------- DATATYPE

; El datatype exp-arit, define los predicados, constructores y extractores.
; EL datatype rand, define los operadores permitidos dentro de la expresion
; aritmetica 

;;;; Definicion Operandos

;Predicados de los operandos permitidos
(define suma? 
  (lambda (sym)
    (if (equal? sym '+) #t #f)
    ))

(define resta? 
  (lambda (sym)
    (if (equal? sym '-) #t #f)
    ))

(define multiplicacion? 
  (lambda (sym)
    (if (equal? sym '*) #t #f)
    ))

(define division? 
  (lambda (sym)
    (if (equal? sym '/) #t #f)
    ))

; Proposito:
; Definir los perandos permitidso para realizar
; las expresiones aritmeticas

(define-datatype rand rand?
  (sum-type (s suma?))
  (rest-type (r resta?))
  (multi-type (m multiplicacion?))
  (div-type (d division?))
 )


; Validar si el simbolo dado es un operador permitido
(define operando? 
  (lambda (sym)
    (cond [(suma? sym) #t]
          [(resta? sym) #t]
          [(multiplicacion? sym) #t]
          [(division? sym) #t]
          [else #f])
    ))


;;; Definicion Expresión aritmetica

; Implementacion propia de los simbolos permitidos para
; almacenar valores
(define symbol-pro?
  (lambda (letra)
     (and (not (operando? letra)) (symbol? letra))))


; Proposito: implementación de la gramatica
(define-datatype exp-arit exparit?
  (num-type (x number?))
  (sym-type (y symbol-pro?))
  (arit-type (body1 exparit?)
             (rand rand?)
             (body2 exparit?))
  )


;;--------------------UNPARSE

;; unparse-rand
; Proposito:
; que toma una expresión en sintaxis abstracta
; y devuelve su sintaxis concreta
(define unparse-rand
  (lambda (exp)
    (cases rand exp
      (sum-type (s) s)
      (rest-type (r) r)
      (multi-type (m) m)
      (div-type (d) d)
     )
    )
)

;; unparse-exp-arit
; Proposito:
; Toma una expresión en sintaxis abstracta y devuelve su sintaxis concreta
(define unparse-exp-arit
  (lambda (exp)
    (cases exp-arit exp
      (num-type (x) x)
      (sym-type (y) y)
      (arit-type (body1 rand body2)
                 (list (unparse-exp-arit body1)
                       (unparse-rand rand)
                       (unparse-exp-arit body2)))
      
      )))

(define arit1 (arit-type (num-type 5) (rest-type '-) (num-type 8)))
(define arit2 (arit-type (arit-type (num-type 5) (multi-type '*) (num-type 5)) (rest-type '-) (num-type 8)))

(unparse-exp-arit arit1)
(unparse-exp-arit arit2)

;;-------------- VERIFICADOR de sintaxis concreta

;; exp-arit?
; Proposito: Toma una expresión y dice
; si es aritmética o no

(define exp-arit?
  (lambda (exp-concreta)
    (cond [(number?  exp-concreta) #t]
          [(symbol-pro? exp-concreta) #t]
          [(operando? exp-concreta) #f]
          [else (and (exp-arit? (car exp-concreta))
                     (operando? (cadr exp-concreta)) ; Siempre despues de un numero o simbolo debe haber un operador
                     (exp-arit? (caddr exp-concreta)))]
          )))


(exp-arit? '((7 - x) - (9 + 6))) ; #t
(exp-arit? '(* - *))  ; #f
(exp-arit? '(8 - *))  ; #f
(exp-arit? '(/ - (2 - 5))) ; #f
(exp-arit? '((7 - x) - 5)) ; #t
(exp-arit? '(8 + (7 - x))) ; #t
(exp-arit? '((8 - 5) - *))  ; #f


;;------- Ambientes - Representación como listas 
; Proposito: poder ligar un valor a un simbolo, y poder usarlos en la expresión

;; empty-env 
(define empty-env
  (lambda () (list 'empty-env)))

;; extend-env: Externder el ambiente
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

;; apply-env : Buscar y devolver el valor ligado a un simbolo
(define apply-env
  (lambda (env search-var)
    (cond ((eqv? (car env) 'empty-env)
           (eopl:error 'apply-env "No binding for ~s" search-var))
          ((eqv? (car env) 'extend-env)
           (let ((saved-var (cadr env))
                 (saved-val (caddr env))
                 (saved-env (cadddr env)))
             (if (eqv? search-var saved-var)
                 saved-val
                 (apply-env saved-env search-var))))
          (else (eopl:error 'apply-env "Expecting an environment, given ~s" env)))))


;; AMBIENTE INICIAL

(define env
  (extend-env 'x 7
              (extend-env 'y 14
                          (extend-env 'z 9 (empty-env)))))


(apply-env env 'x)
(apply-env env 'y)


;;; ---------------- EVALUAR EXPRESION ARITMETICA

;; eval-exp-arit
; Proposito: que toma una expresión aritmética,
; la evalúa, la opera y retornando el resultado
(define eval-exp-arit
  (lambda (exp)
    (letrec
        (;ZONA DECLARACION
         (aux-eval-exp
          (lambda (arit)
            (cond [(number?  arit) arit]
                  [(symbol-pro? arit) (apply-env env arit)]
                  [(suma? arit) +]
                  [(resta? arit) -]
                  [(multiplicacion? arit) *]
                  [(division? arit) /]
                  [else ((aux-eval-exp (cadr arit)) ; Operador
                         (aux-eval-exp (car arit)) ; Operando1 
                         (aux-eval-exp (caddr arit))) ; Operando2 
                        ]
                  )))
         )
      
      ;ZONA EJECUCION
      (if (exp-arit? exp)
          (aux-eval-exp exp)
          "Expresion erronea"
          )
      
      )))


(eval-exp-arit '(5 / 5)) ; 1
(eval-exp-arit '((6 + 4) + (19 * 2))) ; 48
(eval-exp-arit '(x - y)) ; -7
(eval-exp-arit '(x + (y * z))); 133
(eval-exp-arit '(5 * (8 + 2))) ; 50
(eval-exp-arit '(* - (8 + 2))) ; Expresion erronea


;; ---------- PARSER

; Proposito: Convertir de syntaxis concreta a abstracta los operadores dados
; Parser de operandos
(define parse-rand
  (lambda (concreta)
    (cond [(suma?  concreta) (sum-type  concreta)]
          [(resta? concreta) (rest-type concreta)]
          [(multiplicacion?  concreta) (multi-type concreta)]
          [(division? concreta) (div-type concreta)]
          )))

; Proposito:Convertir de syntaxis concreta a abstracta la expresion aritmetica
(define parse
  (lambda (exp-concreta)
    (cond [(number?  exp-concreta) (num-type  exp-concreta)]
          [(symbol-pro? exp-concreta) (sym-type exp-concreta)]
          [(operando?  exp-concreta) (parse-rand exp-concreta)]
          [else (arit-type (parse (car exp-concreta))
                           (parse (cadr exp-concreta))
                           (parse (caddr exp-concreta)))]
          )))


(parse '(5 + 8))
(parse '((5 / x) * 8))
(parse '(5 - (69 + y)))

