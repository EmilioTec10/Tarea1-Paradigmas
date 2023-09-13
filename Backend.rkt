#lang racket

(provide add-edge)
(provide get-weight)
(provide create-node)
(provide add-node-to-graph)
(provide get-by-index)
(provide shortest_path)

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

(define (functional-route end route)
  (equal? end (car route)))

(define (functional-route-with-weight end route)
  (equal? end (caar route)))

(define (neighbors element graph)
  (neighbors-aux (assoc element graph) element graph))

(define (neighbors-aux result element graph)
  (cond ((equal? result #f) #f)
        (else
         (cond ((null? (cdr result)) (cdr result))
               (else (cadr result))))))

(define (node-part-route? ele list)
  (cond ((null? list) #f)
        ((equal? ele (car list)) #t)
        (else (node-part-route? ele (cdr list)))))

(define (node-part-route?_weight ele list)
  (cond ((null? list) #f)
        ((equal? ele (caar list)) #t)
        (else (node-part-route?_weight ele (cdr list)))))

(define (neighbors_nodes route graph)
  (neighbors_nodes_aux route '() graph (neighbors (car route) graph)))



(define (neighbors_nodes_aux route generated_route graph neighbors)
  (cond ((null? neighbors) generated_route)
        (else
         (cond ((node-part-route? (caar neighbors) route)
                (neighbors_nodes_aux route generated_route graph (cdr neighbors)))
               (else
                (neighbors_nodes_aux
                 route
                 (append (list (cons (caar neighbors) route)) generated_route)
                 graph
                 (cdr neighbors)))))))


(define (neighbors_nodes_weight route graph)
  (neighbors_nodes_weight_aux route '() graph (neighbors (caar route) graph)))


(define (neighbors_nodes_weight_aux route generated_route graph neighbors)
  (cond ((null? neighbors) generated_route)
        (else
         (cond ((node-part-route?_weight (car neighbors) route)
                (neighbors_nodes_weight_aux route generated_route graph (cdr neighbors)))
               (else
                (neighbors_nodes_weight_aux
                 route
                 (append (list (cons (car neighbors) route)) generated_route)
                 graph
                 (cdr neighbors)))))))

;Esta función revierte una list de routes para que estén en el orden correcto.
(define (reverts_elem routesAux routes)
  (cond ((null? routes) routesAux)
        (else (reverts_elem
               (append (list (reverse (car routes))) routesAux)
               (cdr routes)))))
;Esta función busca un camino desde un nodo de inicio hasta un nodo de destino en el grafo.
;Utiliza una list de routes para llevar un registro de los caminos posibles.
(define (exist_path begin end graph)
  (exist_path_aux (list (list begin)) end graph '()))

(define (exist_path_aux routes end graph total)
  (cond ((null? routes) (reverts_elem '() total))
        ((functional-route end (car routes))
         (exist_path_aux
          (cdr routes)
          end
          graph
          (cons (car routes) total)))
        (else
         (exist_path_aux
          (append (neighbors_nodes (car routes) graph) (cdr routes))
          end
          graph
          total))))

(define (exist_path_weight begin end graph)
  (exist_path_weight_aux (list (list (list begin '0))) end graph '()))

(define (exist_path_weight_aux routes end graph total)
  (cond ((null? routes) (reverts_elem '() total))
        ((functional-route-with-weight end (car routes))
         (exist_path_weight_aux
          (cdr routes)
          end
          graph
          (cons (car routes) total)))
        (else
         (exist_path_weight_aux
          (append (neighbors_nodes_weight (car routes) graph) (cdr routes))
          end
          graph
          total))))

(define (distance_between_routes routes)
  (distance_between_routes_aux routes '()))

(define (distance_between_routes_aux routes listaTotales)
  (cond ((null? routes) listaTotales)
        (else
         (distance_between_routes_aux
          (cdr routes)
          (append listaTotales (list (totalDistanceRoute 0 (car routes))))))))

(define (totalDistanceRoute num route)
  (cond ((null? route) num)
        (else 
          (let ((peso (cadar route)))
            (if (string? peso)
                (totalDistanceRoute (+ num (string->number peso)) (cdr route))
                (totalDistanceRoute (+ num peso) (cdr route)))))))

(define (least_result list)
  (least_result_aux list (car list) 0))

(define (least_result_aux list num counter)
  (cond ((null? list) counter)
        (else
         (cond ((<= num (car list))
                (least_result_aux (cdr list) num counter))
               (else
                (least_result_aux (cdr list) (car list) (+ counter 1)))))))




(define (shortest_path begin end graph)
  (cond ((not (valid_node? begin graph)) => (lambda (_) "Nodo de inicio invalido"))
        ((not (valid_node? end graph)) => (lambda (_) "Nodo de end invalido"))
        (else
         (shortest_path_aux
          (exist_path_weight begin end graph)
          (exist_path begin end graph)))))

(define (valid_node? nodo graph)
  (not (null? (assoc nodo graph))))

(define (shortest_path_aux routes routesWithoutWeight)
  (cond ((null? routes) "No hay camino existente")
        (else
         (camino_mas_corto_aux2
          (least_result (distance_between_routes routes))
          routes
          routesWithoutWeight))))

(define (camino_mas_corto_aux2 num routes routesWithoutWeight)
  (cond ((zero? num)
         (cons (car routesWithoutWeight) (list (totalDistanceRoute 0 (car routes)))))
        (else
         (camino_mas_corto_aux2
          (- num 1)
          (cdr routes)
          (cdr routesWithoutWeight)))))

