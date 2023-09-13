#lang racket

(provide add-edge)
(provide get-weight)
(provide create-node)
(provide add-node-to-graph)
(provide get-by-index)
(provide camino_mas_corto)

;Adds a node to the graph
(define (add-node-to-graph graph new-node)
  (cons new-node graph))

;Create a node by its name
(define (create-node name)
(list name '()))

;Get the weight of an edge
(define (get-weight node graph)
  (cdr (assoc node graph)))

;Adds an edge to the graph
(define (add-edge graph origin-node destiny-node weight)
  (define (add-aux actual-graph) ;Auxiliar function that helps to get the actual graph to analize
    (cond
      ((null? actual-graph) (list (list origin-node destiny-node weight)))
      ((and 
      (empty? (cadr(car actual-graph))) ;Checks if the node has any edges
        (cons (caar actual-graph) (list (list destiny-node weight))) ;In case it is empty puts the first edge in the list of edges of the node
      (string=?  (caar actual-graph) origin-node)) ;Checks if the origin node is equal to the firts node in the actual graph
        (list(append (cons (caar actual-graph) (list(list (list destiny-node weight)))) (cdr actual-graph)))) ;In case is true it adds the new edge into the list of edges
      ((string=?  (car (car actual-graph)) origin-node) ;Else if checks if the origin node is equal to the firts node in the actual graph
        (display (cons(caar actual-graph) (list(list destiny-node weight))))
        (cons (append (list (caar actual-graph)) (list(append (cadr(car actual-graph)) (list (list destiny-node weight))))) (cdr actual-graph)));Adds the edge in the list of edges in the node
      (else
        (cons (car actual-graph) (add-aux (cdr actual-graph)))))) ;recursive call with the rest of the graph

  (add-aux graph))

;Gets an element of a list by its index
(define (get-by-index list index)
  (cond
    [(< index 0) (error "Index out of range")]
    [(= index 0) (car list)]
    [else (get-by-index (cdr list) (- index 1))]))

;BFS Algorithm

(define (ruta_funcional fin ruta)
  (equal? fin (car ruta)))

(define (ruta_funcional_con_peso fin ruta)
  (equal? fin (caar ruta)))

(define (vecinos elemento grafo)
  (vecinos_aux (assoc elemento grafo) elemento grafo))

(define (vecinos_aux resultado elemento grafo)
  (cond ((equal? resultado #f) #f)
        (else
         (cond ((null? (cdr resultado)) (cdr resultado))
               (else (cadr resultado))))))

(define (nodo_parte_ruta? ele lista)
  (cond ((null? lista) #f)
        ((equal? ele (car lista)) #t)
        (else (nodo_parte_ruta? ele (cdr lista)))))

(define (nodo_parte_ruta?_peso ele lista)
  (cond ((null? lista) #f)
        ((equal? ele (caar lista)) #t)
        (else (nodo_parte_ruta?_peso ele (cdr lista)))))

(define (nodos_vecinos ruta grafo)
  (nodos_vecinos_aux ruta '() grafo (vecinos (car ruta) grafo)))



(define (nodos_vecinos_aux ruta rutaGenerada grafo vecinos)
  (cond ((null? vecinos) rutaGenerada)
        (else
         (cond ((nodo_parte_ruta? (caar vecinos) ruta)
                (nodos_vecinos_aux ruta rutaGenerada grafo (cdr vecinos)))
               (else
                (nodos_vecinos_aux
                 ruta
                 (append (list (cons (caar vecinos) ruta)) rutaGenerada)
                 grafo
                 (cdr vecinos)))))))


(define (nodos_vecinos_peso ruta grafo)
  (nodos_vecinos_peso_aux ruta '() grafo (vecinos (caar ruta) grafo)))


(define (nodos_vecinos_peso_aux ruta rutaGenerada grafo vecinos)
  (cond ((null? vecinos) rutaGenerada)
        (else
         (cond ((nodo_parte_ruta?_peso (car vecinos) ruta)
                (nodos_vecinos_peso_aux ruta rutaGenerada grafo (cdr vecinos)))
               (else
                (nodos_vecinos_peso_aux
                 ruta
                 (append (list (cons (car vecinos) ruta)) rutaGenerada)
                 grafo
                 (cdr vecinos)))))))

;Esta función revierte una lista de rutas para que estén en el orden correcto.
(define (revierte_elem rutasAux rutas)
  (cond ((null? rutas) rutasAux)
        (else (revierte_elem
               (append (list (reverse (car rutas))) rutasAux)
               (cdr rutas)))))
;Esta función busca un camino desde un nodo de inicio hasta un nodo de destino en el grafo.
;Utiliza una lista de rutas para llevar un registro de los caminos posibles.
(define (existe_camino ini fin grafo)
  (existe_camino_aux (list (list ini)) fin grafo '()))

(define (existe_camino_aux rutas fin grafo total)
  (cond ((null? rutas) (revierte_elem '() total))
        ((ruta_funcional fin (car rutas))
         (existe_camino_aux
          (cdr rutas)
          fin
          grafo
          (cons (car rutas) total)))
        (else
         (existe_camino_aux
          (append (nodos_vecinos (car rutas) grafo) (cdr rutas))
          fin
          grafo
          total))))

(define (existe_camino_peso ini fin grafo)
  (existe_camino_peso_aux (list (list (list ini '0))) fin grafo '()))

(define (existe_camino_peso_aux rutas fin grafo total)
  (cond ((null? rutas) (revierte_elem '() total))
        ((ruta_funcional_con_peso fin (car rutas))
         (existe_camino_peso_aux
          (cdr rutas)
          fin
          grafo
          (cons (car rutas) total)))
        (else
         (existe_camino_peso_aux
          (append (nodos_vecinos_peso (car rutas) grafo) (cdr rutas))
          fin
          grafo
          total))))

(define (distancias_entre_rutas rutas)
  (distancias_entre_rutas_aux rutas '()))

(define (distancias_entre_rutas_aux rutas listaTotales)
  (cond ((null? rutas) listaTotales)
        (else
         (distancias_entre_rutas_aux
          (cdr rutas)
          (append listaTotales (list (distanciaTotalRuta 0 (car rutas))))))))

(define (distanciaTotalRuta num ruta)
  (cond ((null? ruta) num)
        (else 
          (let ((peso (cadar ruta)))
            (if (string? peso)
                (distanciaTotalRuta (+ num (string->number peso)) (cdr ruta))
                (distanciaTotalRuta (+ num peso) (cdr ruta)))))))

(define (menor_resultado lista)
  (menor_resultado_aux lista (car lista) 0))

(define (menor_resultado_aux lista num cont)
  (cond ((null? lista) cont)
        (else
         (cond ((<= num (car lista))
                (menor_resultado_aux (cdr lista) num cont))
               (else
                (menor_resultado_aux (cdr lista) (car lista) (+ cont 1)))))))




(define (camino_mas_corto ini fin grafo)
  (cond ((not (nodo_valido? ini grafo)) => (lambda (_) "Nodo de inicio invalido"))
        ((not (nodo_valido? fin grafo)) => (lambda (_) "Nodo de fin invalido"))
        (else
         (camino_mas_corto_aux
          (existe_camino_peso ini fin grafo)
          (existe_camino ini fin grafo)))))

(define (nodo_valido? nodo grafo)
  (not (null? (assoc nodo grafo))))

(define (camino_mas_corto_aux rutas rutasSinPeso)
  (cond ((null? rutas) "No hay camino existente")
        (else
         (camino_mas_corto_aux2
          (menor_resultado (distancias_entre_rutas rutas))
          rutas
          rutasSinPeso))))

(define (camino_mas_corto_aux2 num rutas rutasSinPeso)
  (cond ((zero? num)
         (cons (car rutasSinPeso) (list (distanciaTotalRuta 0 (car rutas)))))
        (else
         (camino_mas_corto_aux2
          (- num 1)
          (cdr rutas)
          (cdr rutasSinPeso)))))

