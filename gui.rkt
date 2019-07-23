#lang racket
(require 2htdp/image 2htdp/universe "button.rkt")

(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define TOP (/ HEIGHT 10))
(define RIGHT (/ WIDTH 5))
(define BOTTOM(/ HEIGHT 8))
(define E-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene

;; posn: integer integer -> posn
;; Purpose A structure that represents a position
;; - x: The x coordinate
;; - y: The y coordinate
;;(struct posn (x y))

(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
;; **** BUTTONS BELOW ***
(define BTN-ADD-STATE (button 100 25 "Add" "solid" "red" #f (posn (- WIDTH 100) (- CONTROL-BOX-H 60))))
(define BTN-REMOVE-STATE (button 100 25 "Remove" "solid" "red" #f (posn (- WIDTH 100) (- CONTROL-BOX-H 25))))





(define-struct world (string)) ;; TODO add stuff to world

(define INIT-WORLD (make-world "Hello World")) ;; Create the empty world

(define (draw-world w)
  (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
               
               (place-image (create-gui-top) (/ WIDTH 2) (/ TOP 2)
                            (place-image (create-gui-bottom) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                         (draw-button BTN-ADD-STATE
                                                      (draw-button BTN-REMOVE-STATE E-SCENE))))))


;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay
   (text (string-upcase "Input") 24 "Black")
   (rectangle (/ WIDTH 11) TOP "outline" "red")))


;; los-top-label: null -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label)
  (overlay
   (text (string-upcase "List of sigmas goes here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) TOP "outline" "green")))


;; create-gui-bottom: null -> image
;; Purpose: Creates the top of the gui layout
;; create-gui-bottom: null -> image
;; Purpose: Creates the top of the gui layout
(define (create-gui-bottom)
  (overlay/align "left" "middle"
                 (align-items
                  (rules-bottom-label)
                  (lor-bottom-label))
                 (rectangle WIDTH BOTTOM "outline" "transparent")))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules") 24 "Black")
   (rectangle (/ WIDTH 11) BOTTOM "outline" "red")))

; create-gui-top: null -> image
;; Creates the top of the gui layout
(define (create-gui-top)
  (overlay/align "left" "middle"
                 (align-items
                  (top-input-label)
                  (los-top-label))
                 (rectangle WIDTH TOP "outline" "transparent")))

;; align-items image image -> image
;; Purpose: Aligns 2 images next to each other
(define (align-items item1 item2)
  (beside
   item1
   item2))

;; lor-bottom-label: null -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label)
  (overlay
   (text (string-upcase "List of rules will go here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 200) BOTTOM "outline" "green")))


(define (create-gui-left)
  (overlay/align "left" "top"
                 (above/align "left"
                              (state-left-control)
                              (alpha-left-control)
                              (start-left-control)
                              (end-left-control)
                              (rule-left-control))
                 (rectangle 200 HEIGHT "outline" "gray")))


(define (state-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 (/ HEIGHT 5) "outline" "blue")
                 (control-header "State Options")))
                 

(define (alpha-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 (/ HEIGHT 5) "outline" "blue")
                 (control-header "Alpha Options")))


(define (start-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 (/ HEIGHT 5) "outline" "blue")
                 (control-header "Start State")))

(define (end-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 (/ HEIGHT 5) "outline" "blue")
                 (control-header "End State")))

(define (rule-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 (/ HEIGHT 5) "outline" "blue")
                 (control-header "Add Rules")))
  

;; control-header: string -> image
;; Purpose: Creates a header label for right control panel
(define (control-header msg)
  (overlay
   (text (string-upcase msg) 18 "Black")
   (rectangle 200 25 "outline" "white")))


;;(define (start-left-control))
   


(big-bang
    INIT-WORLD
  (on-draw draw-world))