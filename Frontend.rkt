#lang racket/gui
(require "Backend.rkt")

; Create a main window and set their parameters
(define frame (new frame%
                   [label "Wazitico"]
                   [width 1200]
                   [height 690]
                   [stretchable-width #f]
                   [stretchable-height #f]))

; Make the panels shows in horizontal form
(define horizontalBox (new horizontal-panel% [parent frame]))

; Make the two left panels show in vertical form
(define verticalBox (new vertical-panel% [parent horizontalBox]
                                         [min-width 300]))
;------------------------------------------------------------------------------------------------------------------
; Panel that contains insert nodes funtionality
(define InsNodesPanel (new vertical-panel% [parent verticalBox]
                                  [border 20]
                                  [min-height 150]))   

; Add a text field to InsNodes Panel that get names of the Nodes from user
(define nodesNameInput (new text-field% [parent InsNodesPanel]
                                    [label "Inserte los nodos aquí: "]
                                    [font (make-font #:size 16 #:weight 'bold)]
                                    [min-width 280]
                                    [vert-margin 1]))

; Add a horizontal panel to the dialog, with centering for buttons
(define InsNodesPanelAux (new horizontal-panel% [parent InsNodesPanel]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(define cancelBut (new button% [parent InsNodesPanelAux] 
                               [label "Cancelar"]
                               [font (make-font #:size 15 #:weight 'bold)]
                               [min-width 100]
                               [min-height 50]
                               [callback (lambda (button event)
                                  (send nodesNameInput set-value "") )]))

; Create a Button that insert the nodes from the user
(define insertBut (new button% [parent InsNodesPanelAux] 
                               [label "Insertar"]
                               [font (make-font #:size 15 #:weight 'bold)]
                               [min-width 100]
                               [min-height 50]
                               [callback (lambda (button event) ; Button functionality
                                  (define text (send nodesNameInput get-value)) ; Get text from text box input
                                  (when (not (string=? text ""))
                                        (set! nodesList (append nodesList (list text))) ; Store in a list the new nodes
                                        
                                        ;All refer to add the nodes to the graph
                                        (define nodeG (create-node text)) ;Creates the node that will go to the graph
                                        (set! graph (add-node-to-graph graph nodeG))
                                        (display graph)
                                        (newline)
                                        (printf "Inserted: ~a\n" text)                    
                                        (send nodesNameInput set-value "") ; Clear the text field after insert
                                      
                                  (define node (list-ref nodeCoords nodeCounter))
                                  (drawNode (getXCoord node) (getYCoord node) text)
                                  (set! nodeCounter (+ nodeCounter 1))
                                  ; Refresh the choices in choice boxes
                                  (send begginChoiceRel append text) 
                                  (send endChoiceRel append text) 
                                  (send begginChoiceDir append text) 
                                  (send endChoiceDir append text))) 
                                ]))  

; Initialize an empty list to store inserted nodes
(define nodesList '()) ;when contains items looks like '("Rusia" "Brazil" "France")

(define relationsList '()) ; ( (0 1 25) (2 5 30) (2 3 10) )

(define graph '())

;-------------------------------------------------------------------------------------------------------------------                                  

; Panel that contains insert relations between nodes functionality
(define insRelPanel (new vertical-panel% [parent verticalBox]
                                [border 50]
                                [min-height 300]))

; Title of the section of relations
(define relTitle (new message% [parent insRelPanel]
                        [label "Inserte las relaciones entre nodos aquí"]
                        [font (make-font #:size 16 #:weight 'bold)]
                        [vert-margin 3]))

; First choices box to indicate the beggin node
(define begginChoiceRel (new choice%
                    [label "Inicio: "]
                    [parent insRelPanel]
                    [choices nodesList]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

; Second choices box to indicate the destiny node
(define endChoiceRel (new choice%
                    [label "Destino:  "]
                    [parent insRelPanel]
                    [choices nodesList]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

; Input text field to indicate the weight or distance of a relation path
(define distanceInput (new text-field% [parent insRelPanel]
                                    [label "Distancia entre Nodos: "]
                                    [font (make-font #:size 14 #:weight 'bold)]
                                    [min-width 100]
                                    [vert-margin 3]))

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Falta Funcionalidad <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
(define relButton (new button%
                   [parent insRelPanel]
                   [label "Insertar"]
                   [vert-margin 3]
                   [font (make-font #:size 15 #:weight 'bold)]
                   [min-width 100]
                   [min-height 50]
                   [callback (lambda (button event)
                               (define beggin (send begginChoiceRel get-selection))
                               (define destiny (send endChoiceRel get-selection))
                               (define weight (send distanceInput get-value))
                               (when (and (not (string=? weight "")) (not(equal? beggin destiny)))
                                        (define relation (list beggin destiny (string->number weight)))
                                        (set! relationsList (cons relation relationsList)) ; Store in a list the new nodes
                                        (display nodesList)
                                        (newline)
                                        (display destiny)
                                        (newline)
                                        (set! graph (add-edge graph (get-by-index nodesList beggin) (get-by-index nodesList destiny) weight)) ;Creates the edge that it will be added to the graph
                                        (display graph)

                                        (send distanceInput set-value "") ; Clear the text field after insert
                                        (define begginXCoord (getPointXCoord (list-ref nodeCoords beggin)))
                                        (define begginYCoord (getPointYCoord (list-ref nodeCoords beggin)))
                                        (define endXCoord (getPointXCoord (list-ref nodeCoords destiny)))
                                        (define endYCoord (getPointYCoord (list-ref nodeCoords destiny)))
                                        (drawLine begginXCoord begginYCoord endXCoord endYCoord)
                                        (writeText weight (/ (+ begginXCoord endXCoord) 2) (/ (+ begginYCoord endYCoord) 2))))]))

;------------------------------------------------------------------------------------------------------------------                                

; Panel that contains insert relations between nodes functionality
(define getDirPanel (new vertical-panel% [parent verticalBox]
                                [border 10]
                                [min-height 300]))

(define dirTitle (new message% [parent getDirPanel]
                        [label "Calcule las rutas aquí"]
                        [font (make-font #:size 16 #:weight 'bold)]))

(define begginChoiceDir (new choice%
                    [label "Inicio:  "]
                    [parent getDirPanel]
                    [choices nodesList]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

(define endChoiceDir (new choice%
                    [label "Destino:  "]
                    [parent getDirPanel]
                    [choices nodesList]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Falta Funcionalidad <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
(define dirbutton (new button%
                   [parent getDirPanel]
                   [label "Calcular Ruta"]
                   [vert-margin 3]
                   [font (make-font #:size 15 #:weight 'bold)]
                   [min-width 100]
                   [min-height 50]
                   [callback (lambda (button event)
                              (newline)
                              (display (shortest_path (get-by-index nodesList (send begginChoiceDir get-selection )) (get-by-index nodesList(send endChoiceDir get-selection)) graph)))]))

; Reset the graph and all the variables
(define deleteGraph (new button%
                   [parent getDirPanel]
                   [label "Eliminar Mapa"]
                   [vert-margin 50]
                   [font (make-font #:size 15 #:weight 'bold)]
                   [min-width 100]
                   [min-height 50]
                   [callback (lambda (button event)
                               (set! nodesList '())
                               (set! relationsList '())
                               (send dc erase)
                               (set! nodeCounter 0)
                               (send begginChoiceRel clear)
                               (send endChoiceRel clear)
                               (send begginChoiceDir clear)
                               (send endChoiceDir clear))]))

;------------------------------------------------------------------------------------------------------------------                                

; Panel that contains graph graphic representation
(define drawGraphPanel (new panel% [parent horizontalBox]
                                   [min-width 900]))


;Create the canvas where the graph stands
(define graphCanvas (new canvas% [parent drawGraphPanel]))

; Define the dc to draw over the canavs
(define dc (send graphCanvas get-dc))

; Function for draw a node (circle and title)
(define (drawNode x y title)
      (send dc set-brush "red" 'solid) ; set pincel to red color
      (send dc draw-ellipse x y 130 130) ; use racket ellipse function to draw a circle
      (send dc set-text-foreground "black") 
      (send dc set-font (make-font #:size 13 #:weight 'bold))
      (send dc draw-text title (+ x 25) (+ y 50)) ; Write a name in middle of the circle X Y
)

(define (drawLine x1 y1 x2 y2)
      (send dc set-pen "black" 2 'solid) ; Change the color to black
      (send dc draw-line x1 y1 x2 y2)
)

(define (writeText text x y)
      (send dc set-text-foreground "blue") 
      (send dc set-font (make-font #:size 11 #:weight 'bold))
      (send dc draw-text text x y) ; Write a name in middle of the circle X Y
)

(define nodeCounter 0)

; Matrix with the coords of the nodes
(define nodeCoords '((255 20 320 150) ; node 0 (x1 y1 pointX1 pointY1)
                     (515 20 580 150) ; node 1 
                     (720 170 720 235) ; node 2
                     (720 430 720 495) ; node 3
                     (515 620 580 620) ; node 4
                     (255 620 320 620) ; node 5
                     (50 430 180 495) ; node 6
                     (50 170 180 235))) ; node 7

; Function to take a element in a list, given a position
(define (getXCoord lst)
      (list-ref lst 0)) ; Return the element at the specified position

(define (getYCoord lst)
      (list-ref lst 1)) ; Return the element at the specified position

(define (getPointXCoord lst)
      (list-ref lst 2)) ; Return the element at the specified position

(define (getPointYCoord lst)
      (list-ref lst 3)) ; Return the element at the specified position

;---------------------------------------------------------------------------------------------------------------
(send frame show #t)