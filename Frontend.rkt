#lang racket/gui

; Create a main window and set their parameters
(define frame (new frame%
                   [label "Simple GUI"]
                   [width 1200]
                   [height 700]
                   [stretchable-width #f]
                   [stretchable-height #f]))

; Make the panels shows in horizontal form
(define horizontalBox (new horizontal-panel% [parent frame]))

; Make the two left panels show in vertical form
(define verticalBox (new vertical-panel% [parent horizontalBox]
                                         [min-width 300]))

; Panel that contains insert nodes funtionality
(define InsNodesPanel (new vertical-panel% [parent verticalBox]
                                  [border 100]
                                  [min-height 350]))

; Panel that contains insert relations between nodes functionality
(define InsRelPanel (new panel% [parent verticalBox]
                                [border 100]
                                [min-height 350]))

; Panel that contains graph graphic representation
(define drawGraphPanel (new panel% [parent horizontalBox]
                                   [min-width 900]))

; Add a text field to InsNodes Panel that get names of the Nodes from user
(define text-field (new text-field% [parent InsNodesPanel]
                                    [label "Insert your nodes here: "]
                                    [font (make-font #:size 15 #:weight 'bold)]))
 
; Add a horizontal panel to the dialog, with centering for buttons
(define panel (new horizontal-panel% [parent InsNodesPanel]
                                     [alignment '(center center)]))
 
; Add Cancel and Ok buttons to the horizontal panel
(new button% [parent panel] [label "Cancel"])
(new button% [parent panel] [label "Ok"])
(when (system-position-ok-before-cancel?)
  (send panel change-children reverse))

(define world-message (new message% [parent InsRelPanel]
                        [label "World"]))

; Draw a red circle in the right panel
(define canvas (new canvas% [parent drawGraphPanel]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-brush "red" 'solid) ; set pincel to red color
                       (send dc draw-ellipse 100 100 200 200) ; use racket ellipse function to draw a circle
                       (send dc set-text-foreground "black") 
                       (send dc set-font (make-font #:size 17 #:weight 'bold))
                       (send dc draw-text "Canada" 160 185))])) ; Write a name in middle of the circle X Y

(send frame show #t)








