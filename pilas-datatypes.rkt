
#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastian Tamayo - 2110331


;; Gramática
; <stack> ::= <empty-stack> (empty)
;         ::= <push> (<scheme-value> <stack>)


;; DATATYPES

;Procedimiento: definir el tipo de datos y sus producciones
(define-datatype stack stack?
  (empty-stack)
  (push (valor scheme-value?)
       (body stack?)))


;PREDICADOS

;Procedimientos: los dos tipos de datos permitidos son
; numeros y simbolos
(define scheme-value?
  (lambda (valor)
    (if (or (symbol? valor)
            (number? valor))
         #t #f)))

;Procedimientos: verificar si el stack contiene o no datos
; retorna un booleano
(define empty-stack?
  (lambda (exp)
    (cases stack exp
      (empty-stack () #t)
      (push (valor body) #f))))


; EXTRACTORES

; Procedimiento
; Retornar el primer elemento del stack, sin borrrarlo
(define top
  (lambda (exp)
    (cases stack exp
      (empty-stack () "stack is null")
      (push (valor body) valor))))

;Procedimiento:
; Eliminar el primer elemento del stack, y retornar el stack
; donde se había aplicado el valor

(define pop
  (lambda (exp)
    (cases stack exp
      (empty-stack () empty)
      (push (valor body) body))))

;; UNPARSE
; Convertir el datatypes stack, en una lista
; basada en la gramatica
(define unparse
  (lambda (exp)
    (cases stack exp
      (empty-stack () empty)
      (push (valor body)
            (list 'pushItem valor (unparse body))
            ))))

;; PARSE
; Convertir una lista en un arbol de sintaxis abstracta
; con base en el datatypes stack
(define parse
  (lambda (exp)
    (cond [(null? exp) (empty-stack)]
          [(eqv? (car exp) 'pushItem)
           (push (cadr exp) (parse (caddr exp)))]
          )
    ))

;-------------------------------------------
;; PRUEBAS

;;CONSTRUCTORES

; Stack vacio
(empty-stack)

; Push, creación de un stack 
(define stack-test
  (push 's
        (push 5
              (push 't
                    (push 9 (empty-stack))))))

;;PREDICADOS

; Scheme-value?
(scheme-value? +); return false
(scheme-value? 'd); return true
(scheme-value? 5) ; return true

;Empty-stack?
(empty-stack? (empty-stack)) ;Retorna true
(empty-stack? (pop(pop stack-test))) ;Retorna false
(empty-stack? (pop(pop (pop (pop stack-test))))) ; Retorna true


;;EXTRACTORES

;Top
(top stack-test) ; retorna 's
(top (pop stack-test)) ; retorna 'f

;Pop
(pop (empty-stack)) ;retorna '()
(pop (pop stack-test)) ; retorna arbol de sintaxis abstracta

;; UNPARSE
(unparse (empty-stack)) ; retorna una lista vacia
(unparse stack-test) ; retorna una lista con los valores que tenia el stack

;; PARSE
(parse (unparse (empty-stack))) ; retorna arbol de sintaxis abstracta
(parse(unparse stack-test))