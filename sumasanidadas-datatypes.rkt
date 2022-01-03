#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastian Tamayo - 2110331


;; Gramática
;<suma-anidada> ::= <valor-type> ('valor <int>)
;               ::= <suma-type> ('suma <suma-anidada> <suma-anidada>)


;; DATATYPE
; El datatype suma-anidaa, define los predicados,
; constructores y extractores

(define-datatype suma-anidada suma-anidada?
  (valor-type (x number?))
  (suma-type  (body1 suma-anidada?)
         (body2 suma-anidada?))
  )


;; CODIGO CLIENTE

; Proposito:
; Toma una suma-anidada, que este bien contruida 
; y sumar sus valores

(define sumar
  (lambda (exp)
    (cases suma-anidada exp
      (valor-type (valor) valor)
      (suma-type (body1 body2)
                 (+ (sumar body1) (sumar body2)))
      )))


;; UNPARSE
; Proposito;
; Toma una suma-anidada y la convierte en una lista, validando
; sus producciones

(define unparse
  (lambda (exp)
    (cases suma-anidada exp
      (valor-type (valor) (list 'valor valor))
      (suma-type (body1 body2)
            (list 'suma (unparse body1) (unparse body2))))))



;; PARSE
; Proposito:
; Toma una lista y la parsea a un arbol de sintaxis abstracta
; definida en el datatype suma-anidada

(define parse
  (lambda (list-unparse)
    (cond
      [(number? (cadr list-unparse)) (valor-type (cadr list-unparse))]
      [(pair? list-unparse)
       (if  (eqv? (car list-unparse) 'suma)
            (suma-type (parse (cadr list-unparse))
                      (parse (caddr list-unparse)))
            ('invalid)
            )]
      (else 'invalid))))


; ----------------------------
;; PRUEBAS

; UNPARSE
;ejemplo de sumas-anidadas, se crean con sintaxis abstracta, el unparse
; los convierte en sintaxis concreta y lo asigna a las variables

(define unparse1
  (unparse (suma-type (valor-type 8)
                    (suma-type (suma-type
                                (valor-type 25)
                                (valor-type 5))
                               (valor-type 6))))
  )

(define unparse2
  (unparse (suma-type (valor-type 4) (valor-type 3)))
  )


;Pruebas: 
(parse unparse1)
(parse unparse2)
