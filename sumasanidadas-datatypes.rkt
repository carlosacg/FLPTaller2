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
; El tipo de dato suma-anidada  es el constructor
; "valor"; es un numero
; "suma"; espera  2 elementos, de tipo suma-anidada
; suma-anidada?: valida si el dato dado es una suma-anidada

(define-datatype suma-anidada suma-anidada?
  (valor-type (x number?))
  (suma-type  (body1 suma-anidada?)
         (body2 suma-anidada?))
  )


;; unparseTree: exp {suma-anidada} -> list
; Proposito;
; Toma una suma-anidada y la convierte en una lista, validando
; cada producion de la gramatica

(define unparse
  (lambda (exp)
    (cases suma-anidada exp
      (valor-type (x) (list 'valor x))
      (suma-type (body1 body2)
            (list 'suma (unparse body1) (unparse body2))))))


;ejemplo de sumas-anidadas
(define suma1
  (suma-type (valor-type 8)
             (suma-type (suma-type (valor-type 25) (valor-type 5))
                        (valor-type 6)))
 )

(define suma2
  (suma-type (valor-type 4) (valor-type 3))
  )


;Pruebas:
(unparse suma1)
(unparse suma2)


;; parseTree: lista {list} -> {suma-anidada}
; Proposito:
; Toma una lista y la parsea a un arbol de sistacxis abstracta
; definido como suma-anidada

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


; Ejemplo de lista unparsed
(define unparsed1 '(suma (valor 8)(suma (suma (valor 25) (valor 5)) (valor 6))))
(define unparsed2 '(suma (valor 4) (valor 3)))
(define unparsed3 '(suma (suma (valor 5) (valor 8)) (valor 3)))

;Pruebas: 
(parse unparsed1)
(parse unparsed2)
(parse unparsed3)
