#lang racket

(provide agregar-arista)
(provide agregar-nodo)
(provide obtener-pesos)
(provide create-node)
(provide agregar-nodo-al-grafo)
(provide obtener-por-indice)

(define (agregar-nodo grafo nodo)
  (cons (cons nodo '(())) grafo))

(define (agregar-nodo-al-grafo grafo nuevo-nodo)
  (cons nuevo-nodo grafo))

(define (create-node name)
(list name '()))

(define (obtener-pesos nodo grafo)
  (cdr (assoc nodo grafo)))


(define (agregar-arista grafo nodo-origen nodo-destino peso)
  (define (agregar-aux grafo-actual) 
    (cond
      ((null? grafo-actual) (list (list nodo-origen nodo-destino peso)))
      ((and(empty? (car(cdr(car grafo-actual)))) (cons (caar grafo-actual) (list (list nodo-destino peso)))(string=?  (car (car grafo-actual)) nodo-origen))
      (list(append (cons (caar grafo-actual) (list(list (list nodo-destino peso)))) (cdr grafo-actual))))
      ((string=?  (car (car grafo-actual)) nodo-origen)
       (display (cons(caar grafo-actual) (list(list nodo-destino peso))))
       (cons (append (list (caar grafo-actual)) (list(append (car(cdr(car grafo-actual))) (list (list nodo-destino peso))))) (cdr grafo-actual)))
      (else
       (cons (car grafo-actual) (agregar-aux (cdr grafo-actual))))))

  (agregar-aux grafo))


(define (obtener-por-indice lista indice)
  (cond
    [(< indice 0) (error "Índice fuera de rango")]
    [(= indice 0) (car lista)]
    [else (obtener-por-indice (cdr lista) (- indice 1))]))

;(define grafo-con-arista (agregar-arista grafo-con-nuevo-nodo 6 1 4))



