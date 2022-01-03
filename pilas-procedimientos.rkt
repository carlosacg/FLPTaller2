#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastian Tamayo - 2110331


;; Gramática
; <stack> ::= (empty-stack)
;         ::= (<scheme-value> <stack>)



;;CONSTRUCTORES

;; Procedimientos: define una pila vacia y retorna un procedimiento
; que recibe un identificador y emite una respuesta segun él 
; signal empty-stack :
; 1- retorna empty,
; 2- retorna 'stack is null
; 3- booleano con valor true, la pila esta vacia

(define empty-stack
  (lambda ()
    (lambda (signal)
      (cond [(= signal 1) empty ]
            [(= signal 2) "stack is null"]
            [(= signal 3) #t]))))

;; Procedimientos: añade un valor al stack
; que recibe un elemento a añadir, y el stack donde se pondrá
; signal push:
; 1- retorna el stack donde se añadio,
; 2- retorna el elemento del stack
; 3- boolean co valor false, indica la pila vacia
(define push
  (lambda (valor stack)
    (lambda (signal)
      (cond [(= signal 1) stack]
            [(= signal 2) valor]
            [(= signal 3) #f]))))

;; PREDICADOS

;Procedimiento:
; Determina si el stack se encuentra vacio
; llamando al procedimiento con el id 3
(define empty-stack?
  (lambda (stack)
    (stack 3)))


;; EXTRACTORES

;Procedimiento:
; Eliminar el primer elemento del stack, y retornar el stack restante
; llama al procedimiento con el valor de id 1, que retorna el stack que
; acompañaba el elemnto en la pila
(define pop
  (lambda (stack)
    (stack 1)))

; Procedimiento
; Retornar el primer elemento del stack, sin borrrarlo
(define top
  (lambda (stack)
    (stack 2)))


;; PRUEBAS
;;Constructores

; Stack vacio
(empty-stack)

; Push, creación de un stack 
(define stack
  (push 's
        (push 'f
              (push 't (empty-stack)))))

;; Predicado
;Empty-stack?
(empty-stack? (empty-stack)) ;Retorna true
(empty-stack? (pop(pop stack))) ;Retorna false
(empty-stack? (pop(pop (pop stack)))) ; Retorna true

;;Extractores

;Top
(top stack) ; retorna 's
(top (pop stack)) ; retorna 'f

;Pop
(pop (empty-stack)) ;retorna '()
(pop (pop stack)) ; retorna procedimiento
