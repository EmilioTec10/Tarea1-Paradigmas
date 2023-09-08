#lang racket/gui

; Create a main window and set their parameters
(define frame (new frame%
                   [label "Simple GUI"]
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
(define text-field (new text-field% [parent InsNodesPanel]
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
                                  (send text-field set-value "") )]))

; Create a Button that insert the nodes from the user
(define insertBut (new button% [parent InsNodesPanelAux] 
                               [label "Insertar"]
                               [font (make-font #:size 15 #:weight 'bold)]
                               [min-width 100]
                               [min-height 50]
                               [callback (lambda (button event) ; Button functionality
                                  (define text (send text-field get-value)) ; Get text from text box input
                                  (when (not (string=? text ""))
                                        (set! inserted-nodes (cons text inserted-nodes)) ; Store in a list the new nodes
                                        (printf "Inserted: ~a\n" text)
                                        (send text-field set-value "") ; Clear the text field after insert
                                      
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
(define inserted-nodes '()) ;when contains items looks like '("Rusia" "Brazil" "France")

 
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
                    [label "Inicio:  "]
                    [parent insRelPanel]
                    [choices inserted-nodes]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

; Second choices box to indicate the destiny node
(define endChoiceRel (new choice%
                    [label "Destino:  "]
                    [parent insRelPanel]
                    [choices inserted-nodes]
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
                               (display (send begginChoiceRel get-selection) newline))]))

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
                    [choices inserted-nodes]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

(define endChoiceDir (new choice%
                    [label "Destino:  "]
                    [parent getDirPanel]
                    [choices inserted-nodes]
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
                               (display (send begginChoiceDir get-selection) newline))]))

;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> Falta Funcionalidad <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
(define deleteGraph (new button%
                   [parent getDirPanel]
                   [label "Eliminar Mapa"]
                   [vert-margin 50]
                   [font (make-font #:size 15 #:weight 'bold)]
                   [min-width 100]
                   [min-height 50]
                   [callback (lambda (button event)
                               (set! inserted-nodes '())
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

(define nodeCounter 0)

; Matrix with the coords of the nodes
(define nodeCoords '((255 20 180 138) ; node 1
                     (515 20 180 138) ; node 2
                     (720 170 180 138) ; node 3
                     (720 430 180 138) ; node 4
                     (515 620 180 138) ; node 5
                     (255 620 180 138) ; node 6
                     (50 430 180 138) ; node 7
                     (50 170 180 138))) ; node 8

; Function to take a element in a list, given a position
(define (getXCoord lst)
      (list-ref lst 0)) ; Return the element at the specified position

(define (getYCoord lst)
      (list-ref lst 1)) ; Return the element at the specified position

(define (getPointerXCoord lst)
      (list-ref lst 2)) ; Return the element at the specified position

(define (getPointerYCoord lst)
      (list-ref lst 3)) ; Return the element at the specified position

;---------------------------------------------------------------------------------------------------------------
(send frame show #t)