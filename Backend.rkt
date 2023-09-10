#lang racket

;Lines to provide the functions to the frontend
(provide create-node)
(provide create-edge)
(provide make-graph)
(provide node-name)
(provide get-nodes)
(provide add-nodes-to-graph)
(provide add-edges-to-graph)
(provide edge-source)
(provide edge-target)
(provide edge-weight)
(provide get-edges)

;Node and edge struct
(define-struct node (name))
(define-struct edge (source target weight))

;Graph struct
(define-struct graph (nodes edges))

;Function to create a node
(define (create-node name)
  (make-node name))

;Function to create an edge
(define (create-edge source target weight)
  (make-edge source target weight))

;Function to get all the nodes of the graph
(define (get-nodes graph)
  (graph-nodes graph))

;Function to get all the edges of the graph
(define (get-edges graph)
  (graph-edges graph))

;Function to find edges by id
(define (find-edges-by-source edges source-id)
  (filter (λ (edge) (= (edge-source edge) source-id)) edges))

;Function to add nodes to the graph
(define (add-nodes-to-graph graph nodes)
  (make-graph (append nodes (graph-nodes graph)) (graph-edges graph)))

;Function to add nedges to the graph
(define (add-edges-to-graph graph edges)
  (make-graph (graph-nodes graph) (append edges (graph-edges graph))))

#|
;Example
(define my-graph (make-graph '() '()))

(define node1 (create-node "Node A"))
(define node2 (create-node "Node B"))
(define node3 (create-node "Node C"))

(set! my-graph (add-nodes-to-graph my-graph (list node1 node2 node3)))

(define edge1 (create-edge node1 node2 5))
(define edge2 (create-edge node2 node3 3))

(set! my-graph (add-edges-to-graph my-graph (list edge1 edge2)))

(displayln "Aristas iniciales:")
(for-each (λ (edge) (displayln (format "Source: ~a, Target: ~a, Weight: ~a" (edge-source edge) (edge-target edge) (edge-weight edge)))) (get-edges my-graph))

(define node4 (create-node "Node D"))
(define edge3 (create-edge node1 node4 7))

(set! my-graph (add-edges-to-graph my-graph (list edge3)))

(displayln "Todas las aristas:")
(for-each (λ (edge) (displayln (format "Source: ~a, Target: ~a, Weight: ~a" (edge-source edge) (edge-target edge) (edge-weight edge)))) (get-edges my-graph))
|#