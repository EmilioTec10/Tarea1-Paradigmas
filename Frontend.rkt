#lang racket/gui

; Coords for nodes
(define node1 '(180 138 180 138))
(define node2 '(360 138 360 138))
(define node3 '(540 276 540 276))
(define node4 '(720 414 720 414))
(define node5 '(720 552 720 552))
(define node6 '(540 552 540 552))
(define node7 '(360 414 360 414))
(define node8 '(180 276 180 276))

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
                                  [border 50]
                                  [min-height 230]))   

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

                                  ((new canvas% [parent drawGraphPanel]
                                        [paint-callback
                                        (lambda (canvas dc)
                                        (send dc set-brush "red" 'solid) ; set pincel to red color
                                        (send dc draw-ellipse 100 100 200 200) ; use racket ellipse function to draw a circle
                                        (send dc set-text-foreground "black") 
                                        (send dc set-font (make-font #:size 17 #:weight 'bold))
                                        (send dc draw-text text 160 185))]) ; Write a name in middle of the circle X Y)

                                  ; Refresh the choices in choice boxes
                                  (send begginChoiceRel append text) 
                                  (send endChoiceRel append text) 
                                  (send begginChoiceDir append text) 
                                  (send endChoiceDir append text)))) 
                                ]))  

(define inserted-nodes '()) ; Initialize an empty list to store inserted nodes 
 
;-------------------------------------------------------------------------------------------------------------------                                  

; Panel that contains insert relations between nodes functionality
(define insRelPanel (new vertical-panel% [parent verticalBox]
                                [border 50]
                                [min-height 230]))

(define relTitle (new message% [parent insRelPanel]
                        [label "Inserte las relaciones entre nodos aquí"]
                        [font (make-font #:size 16 #:weight 'bold)]
                        [vert-margin 3]))

(define begginChoiceRel (new choice%
                    [label "Inicio:  "]
                    [parent insRelPanel]
                    [choices inserted-nodes]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

(define endChoiceRel (new choice%
                    [label "Destino:  "]
                    [parent insRelPanel]
                    [choices inserted-nodes]
                    [font (make-font #:size 14 #:weight 'bold)]
                    [min-width 100]
                    [vert-margin 3]))

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
                                [border 50]
                                [min-height 230]))
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

;------------------------------------------------------------------------------------------------------------------                                

; Panel that contains graph graphic representation
(define drawGraphPanel (new panel% [parent horizontalBox]
                                   [min-width 900]))


; Draw a red circle in the right panel
(define circles (new canvas% [parent drawGraphPanel]
                                        [paint-callback
                                        (lambda (canvas dc)
                                        (send dc set-brush "red" 'solid) ; set pincel to red color
                                        (send dc draw-ellipse 300 300 100 100) ; use racket ellipse function to draw a circle
                                        (send dc set-text-foreground "black") 
                                        (send dc set-font (make-font #:size 12 #:weight 'bold))
                                        (send dc draw-text "Canada" 310 335))])) ; Write a name in middle of the circle X Y


;---------------------------------------------------------------------------------------------------------------
(send frame show #t)