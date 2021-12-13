#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastián Tamayo - 2110331

;; Gramática
; <stack> := (empty-stack)
;         := (push <scheme-value> <stack>)

; Definición de pila vacía:
; Procedimiento: genera una lista con un valor llamado empty-stack
(define empty-stack
  (lambda () (list 'empty-stack)
    )
  )

; Pruebas
(empty-stack)

; Definición de 3 stacks de prueba
(define my-stack1 (empty-stack))
(define my-stack2 '(1 2 3 empty-stack))
(define my-stack3 '('y 1 "Hola" empty-stack))

; Predicado que se encarga de preguntar si la pila está vacía:
; Procedimiento:
;  Si el primero de la lista es igual a empty-stack, quiere decir que la pila está vacía y devuelve true
;  De lo contrario, quiere decir que tiene algo más además de empty-stack y devuelve false
(define empty-stack?
  (lambda (stack)
    (cond [(eqv? (car stack) 'empty-stack) #t]
          [else #f])
    )
  )

; Pruebas
(empty-stack? my-stack1)
(empty-stack? my-stack2)
(empty-stack? my-stack3)

; Insertar elemento en una pila:
; Procedimiento:
;  Primero valida si es una pila vacía, y devuelve una lista con el elemento y empty-stack
;  De lo contrario devuelve una lista teniendo como primer valor al elemento y como el resto, a la pila entrante
(define push
  (lambda (element stack)
    (cond [(empty-stack? stack) (cons element (empty-stack))]
          [else (cons element stack)]
          )
    )
  )

; Pruebas
(push 'x my-stack1)
(push 'a (push 'b my-stack2))
(push "Hola" (push "mundo" my-stack3))

; Definición de 3 stacks de prueba
(define my-stack4 (push 'x my-stack1))
(define my-stack5 (push 'a (push 'b my-stack2)))
(define my-stack6 (push "Hola" (push "mundo" my-stack3)))

; Retira el elemento superior de la pila:
; Procedimiento:
;  Si la pila está vacía, quiere decir que solo tiene un elemento que es empty-stack, entonces lo elimina
;  Si tiene más de un elemento, devuelve el resto de la lista
(define pop
  (lambda (stack)
    (cond [(empty-stack? stack) empty]
          [else (cdr stack)]
          )
    )
  )

; Pruebas
(pop my-stack1)
(pop my-stack2)
(pop my-stack3)
(pop my-stack4)
(pop my-stack5)
(pop my-stack6)

(push "Hi" (pop my-stack6))
(push 'a (pop my-stack2))

; Devuelve el elemento superior de la pila:
; Procedimiento:
;  Si la pila está vacía, devuelve la pila, que solo contiene un elemento -> empty-stack
;  De lo contrario devuelve el primero de la pila
(define top
  (lambda (stack)
    (cond [(empty-stack? stack) stack]
          [else (car stack)]
          )
    )
  )

; Pruebas
(top my-stack1)
(top my-stack2)
(top my-stack3)
(top my-stack4)
(top my-stack5)
(top my-stack6)

(push (top my-stack6) my-stack3)
(push (top my-stack4) my-stack1)