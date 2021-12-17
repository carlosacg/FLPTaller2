#lang eopl
;; FLP - Taller 2
; Angélica María Agudelo - 1741113
; Carlos Andrés Cuervo - 2010002
; María Paula Mosquera - 2110324
; Sebastián Tamayo - 2110331
; Representación Arbol Binario Listas

;******************************Gramatica*************************************

; <árbol-binario> := ('())
;                 := ( <numero> <árbol-binario> <árbol-binario>)

;----------------------------------------------------------------------------

;; CONSTRUCTORES

;; arbol-vacio : () => bintree
;; crea elementos de tipo bintree empty
(define arbol-vacio (lambda () '()))

;; bintree: int x bintree x bintree => bintree
;; crea elementos de tipo bintree
(define bintree (lambda (int left right) (list int left right)))


;; PREDICADOS

;; arbol-vacio?:: bintree => bool

(define arbol-vacio? (lambda (param) (null? param)))

;; bintree?: bintree => bool
;; retorna si un elemento es de tipo arbol-vacio
(define bintree? (lambda (param)
                   [cond
                    ((arbol-vacio?  param) #t)
                    ((number? (car param)) (and (bintree? (cadr param)) (bintree? (caddr param))))
                    (else  #f)]))

;; Extractores

;; elemento-actualt : bintree => number
;; retorna el valor del la raiz de un arbol
(define elemento-actual
  (lambda (arbol) (car arbol)))
;Pruebas:
(elemento-actual '(12 (1 () ()) (31 () ())))
;(elemento-actual abb1)
;(elemento-actual abb2)

;; mover-nodo-izquierdo : bintree => number
;; retorna el sub arbol izquierdo
(define mover-nodo-izquierdo
  (lambda (arbol) (cadr arbol)))
;Pruebas:
(mover-nodo-izquierdo '(12 (1 () ()) (31 () ())))
;(mover-nodo-izquierdo abb3)

;; mover-nodo-derecho : bintree => number
;; retorna el sub arbol derecho
(define mover-nodo-derecho
  (lambda (arbol) (caddr arbol)))
;Pruebas
(mover-nodo-derecho '(12 (1 () ()) (31 () ())))
(mover-nodo-derecho '(1 () ()))

;; number-to-bintree : number => bintree
;; que recibe un numero y lo transforma a un
;; ́arbol binario en la gramatica utilizada, sin hijos inicialmente.
(define number->bintree
  (lambda(number)
    (list number (arbol-vacio) (arbol-vacio))))

;Pruebas
(number->bintree 8)
(number->bintree 21)

;; arbol-hoja? : bintree => bool
;; pregunta si la posicion actual es un arbol hoja
(define arbol-hoja?
  (lambda (arbol)
    (and (arbol-vacio? (cadr arbol)) (arbol-vacio? (caddr arbol)))))

;Pruebas:
(arbol-hoja? (number->bintree 21))
;(arbol-hoja? abb1)

;; arbol-nodo?: bintree => bool
;; si en donde se encuentra actualmente
;; es un  ́arbol con al menos un hijo
(define arbol-nodo?
  (lambda (arbol)
    (or (not (arbol-vacio? (cadr arbol))) (not (arbol-vacio? (caddr arbol))))))

;Pruebas:
(arbol-nodo? (number->bintree 3))
(arbol-nodo? '(12 (1 () ()) (31 () ())))

;; Funciones

;; validador-orden: bintree => bool
;; valida su propiedad de orden con validaciones anidadas con el uso del AND
;; se recorre todo el arbol validando la propuidad de orde, si todas son verdad retornara verdadero.
(define validador-orden
  (lambda (arbol)
    (and
     (aux (elemento-actual arbol) (mover-nodo-izquierdo arbol) >)
     (aux (elemento-actual arbol) (mover-nodo-derecho arbol) <)
     (if (arbol-vacio? (mover-nodo-izquierdo arbol)) #t (validador-orden (mover-nodo-izquierdo arbol)))
     (if (arbol-vacio? (mover-nodo-izquierdo arbol)) #t (validador-orden (mover-nodo-derecho arbol))))))

;Pruebas:
;(validador-orden abb2)
;(validador-orden abb1)

;; aux: bintree => bool
;; compara sin arbol esta ordenado, con el caso base que un arbol vacio esta trivialmente ordenado.
;; y el caso general que nodo es menor o mayor, que sus hijos izquierdo y derecho respectivamente.
(define aux
  (lambda (cabeza comparar op)
    [if (arbol-vacio? comparar) #t
        (op cabeza (elemento-actual comparar))]))

;; insertar-elemento: bintree x n => bintree
;; navega en el arbol buscando un numero, si lo encuentra retorna el arbol,
;; sino lo agrega en la posicion correcta.
(define insertar-elemento
  (lambda (arbol n)
    (cond
      [(arbol-vacio? arbol)  (number->bintree n)]
      [(= (elemento-actual arbol) n) arbol]
      [(> (elemento-actual arbol) n)
       (bintree
        (elemento-actual arbol)
        (insertar-elemento (mover-nodo-izquierdo arbol) n)
        (mover-nodo-derecho arbol))]
      [else
       (bintree
        (elemento-actual arbol)
        (mover-nodo-izquierdo arbol)
        (insertar-elemento (mover-nodo-derecho arbol) n))])))
      
     
; Pruebas:
;(insertar-elemento abb2 2)
;(insertar-elemento abb5 3)


; Árboles para pruebas
(define abb1 (bintree 1 '() '()))
(define abb2 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(define abb3 (bintree 3 (bintree 1 (bintree 5 '() '()) '()) (bintree 2 '() '())))
(define abb4 (bintree 3 (bintree 1 '() '()) '()))
(define abb5 (bintree 3 '() (bintree 5 '() '()) ))
(define abb6 (bintree 'a (bintree 1 (bintree 5 'b '()) '()) (bintree 2 '() '())))
(define aej '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))