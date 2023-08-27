#lang racket

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

;Function to add a node to the graph
(define (add-node graph node)
  (cons node (graph-nodes graph)))

;Function to add an edge to the graph
(define (add-edge graph edge)
  (cons edge (graph-edges graph)))

;Function to get all the nodes of the graph
(define (get-nodes graph)
  (graph-nodes graph))

;Function to get all the edges of the graph
(define (get-edges graph)
  (graph-edges graph))

;Function to find edges by id
(define (find-edges-by-source edges source-id)
  (filter (λ (edge) (= (edge-source edge) source-id)) edges))

;Example
(define node1 (create-node "Node A"))
(define node2 (create-node "Node B"))
(define node3 (create-node "Node C"))

(define edge1 (create-edge node1 node2 5))
(define edge2 (create-edge node2 node3 3))
(define edge3 (create-edge node1 node3 7))

(define my-graph (make-graph (list node1 node2 node3) (list edge1 edge2 edge3)))

;Show nodes
(displayln "Nodes:")
(for-each (λ (node) (displayln (node-name node))) (get-nodes my-graph))

(displayln "Edges:")
(for-each (λ (edge) (displayln (format "Source: ~a, Target: ~a, Weight: ~a" (edge-source edge) (edge-target edge) (edge-weight edge)))) (get-edges my-graph))
