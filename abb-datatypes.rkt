#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastián Tamayo - 2110331
; Representación Arbol Binario DataTypes

;******************************Gramatica*************************************

; <árbol-binario> := ('())
;                 := ( <numero> <árbol-binario> <árbol-binario>)

;----------------------------------------------------------------------------

;; CONSTRUCTOR
;; crea elementos de tipo bintree
(define-datatype bintree bintree?
  (arbol-vacio)
  (bintree-exp (numero number?) (left bintree?) (right bintree?)))

;;PARSE
;; parse-exp: lista => bintree
;; crear un parse para el tipo de dato bintree
(define parse
  (lambda (L)
    (cond
      [(eqv? L '()) (arbol-vacio)]
      [(number? (car L)) (bintree-exp (car L)(parse (cadr L)) (parse (caddr L)))]      
      [else (eopl:error 'parse "Invalid concret sintax ~s" L)]
      )
    )
  )
;Pruebas:
(parse '(3 (2 () ()) (5 () ())))
;; UNPARSE
;; unparse bintree => lista
;; crear un unparse para el tipo de dato bintree

(define unparse
  (lambda (exp)
    (cases bintree exp
      (arbol-vacio () '())
      (bintree-exp (num left right) (list num (unparse left) (unparse right)))
      (else #f)
      )
    )
  )
;Pruebas:
(unparse (parse '(3 (2 () ()) (5 () ()))))

;; EXTRACTORES

;; arbol-vacio? : item => bool
;; retorna si un elemento es de tipo  arbol-vacio
(define arbol-vacio?
  (lambda (param)
     (cases bintree param
      (arbol-vacio () #t)
      (bintree-exp (n l r) #f))))
;Pruebas:
(arbol-vacio? (arbol-vacio))
(arbol-vacio? (parse '(3 (2 () ()) (5 () ()))))

;; elemento-actual : bintree => number
;; retorna el valor del la raiz de un arbol
(define elemento-actual
  (lambda (arbol)
    (cases bintree arbol
      (arbol-vacio () '())
      (bintree-exp (n l r) n))))

;Pruebas:
(elemento-actual (parse '(3 (2 () ()) (5 () ()))))
(elemento-actual (parse '(12 (1 () ()) (31 () ()))))

;; mover-nodo-izquierdo : bintree => number
;; retorna el sub arbol izquierdo
(define mover-nodo-izquierdo
  (lambda (arbol)
    (cases bintree arbol
      (arbol-vacio () (eopl:error 'move "you can not move to left an item from arbol-vacio"))
      (bintree-exp (n l r) l))))
;Pruebas:
(mover-nodo-izquierdo (parse '(3 (2 () ()) (5 () ()))))
(mover-nodo-izquierdo (parse '(12 (1 () ()) (31 () ()))))
;; mover-nodo-derecho : bintree => number
;; retorna el sub arbol derecho
(define mover-nodo-derecho
 (lambda (arbol)
    (cases bintree arbol
      (arbol-vacio () (eopl:error 'move "you can not move to right an item from arbol-vacio"))
      (bintree-exp (n l r) r))))

;Pruebas:
(mover-nodo-derecho (parse '(3 (2 () ()) (5 () ()))))
(mover-nodo-derecho (parse '(12 (1 () ()) (31 () ()))))

;; number-to-bintree : number => bintree
;; que recibe un numero y lo transforma a un
;; ́arbol binario en la gramatica utilizada, sin hijos inicialmente.
(define number->bintree
  (lambda (numero)
    (bintree-exp numero (arbol-vacio) (arbol-vacio))))

;Pruebas:
(number->bintree 9)
(number->bintree 3)

;; arbol-hoja? : bintree => bool
;; pregunta si la posicion actual es un arbol hoja
(define arbol-hoja?
  (lambda (arbol)
     (cases bintree arbol
      (arbol-vacio () (eopl:error 'move "you can not say arbol-hoja? an item from arbol-vacio")) ;; valid true?
      (bintree-exp (n l r)
                   (and
                    (arbol-vacio? l)
                    (arbol-vacio? r))))))
;Pruebas:
(arbol-hoja? (parse '(3 (2 () ()) (5 () ()))))
(arbol-hoja? (number->bintree 3))

;; arbol-nodo?: bintree => bool
;; si en donde se encuentra actualmente
;; es un  ́arbol con al menos un hijo
(define arbol-nodo?
 (lambda (arbol)
     (cases bintree arbol
      (arbol-vacio () (eopl:error 'move "you can not say arbol-hoja? an item from arbol-vacio"))
      (bintree-exp (n l r)
                   (or (not (arbol-vacio? l))
                       (not (arbol-vacio? r)))))))
;Pruebas:
(arbol-nodo? (parse '(3 (2 () ()) (5 () ()))))
(arbol-nodo? (number->bintree 3))


;; FUNCIONES

;; aux: bintree => bool, compara sin arbol esta ordenado, con el caso base que un arbol vacio esta trivialmente ordenado.
;; y el caso general que nodo es menor o mayor, que sus hijos izquierdo y derecho respectivamente.
(define aux
  (lambda (cabeza comparar op)
    [if (arbol-vacio? comparar) #t
        (op cabeza (elemento-actual comparar))]))


;; validador-orden: bintree => bool
;; valida su propiedad de orden con validaciones anidadas con el uso del AND
;; se recorre todo el arbol validando la propuidad de orde, si todas son verdad retornara verdadero.
(define validador-orden
  (lambda (arbol)
    (and
     (aux (elemento-actual arbol) (mover-nodo-izquierdo arbol) >)
     (aux (elemento-actual arbol) (mover-nodo-derecho arbol) <)
     (if ( arbol-vacio? (mover-nodo-izquierdo arbol)) #t (validador-orden (mover-nodo-izquierdo arbol)))
     (if ( arbol-vacio? (mover-nodo-derecho arbol)) #t (validador-orden (mover-nodo-derecho arbol))))))
;Pruebas:
(validador-orden (parse '(3 (2 () ()) (5 () ()))))
(validador-orden (number->bintree 5))



;; insertar-elemento: bintree x n => bintree
;; navega en el arbol buscando un numero, si lo encuentra retorna el arbol,
;; sino lo agrega en la posicion correcta.

(define insertar-elemento
  (lambda (arbol n)
    (cond
      [(arbol-vacio? arbol)  (number->bintree n)]
      [(= (elemento-actual arbol) n) arbol]
      [(> (elemento-actual arbol) n)
       (bintree-exp
        (elemento-actual arbol)
        (insertar-elemento (mover-nodo-izquierdo arbol) n)
        (mover-nodo-derecho arbol))]
      [else
       (bintree-exp
        (elemento-actual arbol)
        (mover-nodo-izquierdo arbol)
        (insertar-elemento (mover-nodo-derecho arbol) n))])))

(define aej (parse '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ())))))

;Pruebas:
;(insertar-elemento aej 2)
;(insertar-elemento aej 3)