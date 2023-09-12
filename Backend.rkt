#lang racket

(provide add-edge)
(provide get-weight)
(provide create-node)
(provide add-node-to-graph)
(provide get-by-index)

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




