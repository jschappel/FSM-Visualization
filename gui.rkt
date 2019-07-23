#lang racket
(require 2htdp/image 2htdp/universe "button.rkt")

;; GLOBAL VALIRABLES
(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 600) ;; The height of the scene
(define TOP (/ HEIGHT 10))
(define RIGHT (/ WIDTH 5))
(define BOTTOM(/ HEIGHT 8))
(define CONTROL-BOX-H (/ HEIGHT 5)) ;; The height of each left side conrol box
(define E-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene

;; **** BUTTONS BELOW ***
(define BTN-ADD-STATE (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- CONTROL-BOX-H 25))))
(define BTN-REMOVE-STATE (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- CONTROL-BOX-H 25))))

(define BTN-ADD-ALPHA (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- (* 2 CONTROL-BOX-H) 25))))
(define BTN-REMOVE-ALPHA (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 2 CONTROL-BOX-H ) 25))))

(define BTN-ADD-START (button 50 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 71))))
(define BTN-REMOVE-START (button 50 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 3 CONTROL-BOX-H) 25))))

(define BTN-ADD-END (button 50 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 71))))
(define BTN-REMOVE-END (button 50 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 4 CONTROL-BOX-H) 25))))

(define BTN-ADD-RULES (button 70 25 "Add" "solid" (make-color 230 142 174) #f (posn (- WIDTH 150) (- (* 5 CONTROL-BOX-H) 25))))
(define BTN-REMOVE-RULES (button 70 25 "Remove" "solid" (make-color 230 142 174) #f (posn (- WIDTH 50) (- (* 5 CONTROL-BOX-H) 25))))

;; BUTTON-LIST: A List containing all buttons that are displayed on the scene.
(define BUTTON-LIST (list BTN-ADD-STATE BTN-REMOVE-STATE
                          BTN-ADD-ALPHA BTN-REMOVE-ALPHA
                          BTN-ADD-START BTN-REMOVE-START
                          BTN-ADD-END BTN-REMOVE-END
                          BTN-ADD-RULES BTN-REMOVE-RULES))


(define-struct world (string)) ;; TODO add stuff to world

(define INIT-WORLD (make-world "Hello World")) ;; Create the empty world

(define (draw-world w)
  (place-image (create-gui-left) (- WIDTH 100) (/ HEIGHT 2)
               (place-image (create-gui-top) (/ WIDTH 2) (/ TOP 2)
                            (place-image (create-gui-bottom) (/ WIDTH 2) (- HEIGHT (/ BOTTOM 2))
                                         (draw-button BTN-ADD-STATE
                                                      (draw-button BTN-REMOVE-STATE
                                                                   (draw-button BTN-ADD-ALPHA
                                                                                (draw-button BTN-REMOVE-ALPHA
                                                                                             (draw-button  BTN-ADD-START
                                                                                                           (draw-button BTN-REMOVE-START
                                                                                                                        (draw-button BTN-ADD-END
                                                                                                                                     (draw-button BTN-REMOVE-END
                                                                                                                                                  (draw-button  BTN-ADD-RULES
                                                                                                                                                                (draw-button BTN-REMOVE-RULES E-SCENE))))))))))))))


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

;; create-gui-left: null -> image
;; Purpose: creates the left conrol panel for the 
(define (create-gui-left)
  (overlay/align "left" "top"
                 (above/align "left"
                              (state-left-control)
                              (alpha-left-control)
                              (start-left-control)
                              (end-left-control)
                              (rule-left-control))
                 (rectangle 200 HEIGHT "outline" "gray")))

;; state-left-control: null -> image
;; Purpose: Creates the state control panel
(define (state-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "State Options")))

                 
;; alpha-left-control: null -> image
;; Purpose: Creates the alpha control panel
(define (alpha-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Alpha Options")))


;; start-left-control: null -> image
;; Purpose: Creates the start control panel
(define (start-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Start State")))


;; end-left-control: null -> image
;; Purpose: Creates the end control panel
(define (end-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "End State")))

;; rule-left-control: null -> image
;; Purpose: Creates the rule control panel
(define (rule-left-control)
  (overlay/align "left" "top"
                 (rectangle 200 CONTROL-BOX-H "outline" "blue")
                 (control-header "Add Rules")))
  
;; control-header: string -> image
;; Purpose: Creates a header label for right control panel
(define (control-header msg)
  (overlay
   (text (string-upcase msg) 18 "Black")
   (rectangle 200 25 "outline" "transparent")))
   


(big-bang
    INIT-WORLD
  (on-draw draw-world))