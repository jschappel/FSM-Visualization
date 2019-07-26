#lang racket
(require 2htdp/image 2htdp/universe "posn.rkt")

;; ------- BUTTON -------
;; This class holds the buttons functionality and structure


;; Export necessary functions/structures
(provide
 (struct-out button)
 (struct-out posn)
 draw-button
 run-function
 button-pressed?)


;; button: A structurre that represents a button
;; - width: integer representing the width of the button
;; - height: integer representing the height of the button
;; - text: String represting the text to go in the button
;; - type: String representing the type (solid, outline...)
;; - color: String that represents the background color of the button
;; - fontSize: Natural Number that represents the font size
;; - rounded?: Boolean representing the shape of the button. #t if rounded.
;; - location: posn that represents the location for the button
;; - onClick: A function to be run if the button is pressed
(struct button (width height text type color fontSize rounded? location onClick))


;; draw-button: button posn scene -> scene
;; Purpose: Draws a given button onto the scene
(define (draw-button btn scn)
  (place-image (create-button btn) (posn-x (button-location btn)) (posn-y (button-location btn)) scn))

;; create-button: button -> Image
;; Purpose: creates an image that represents the button and then returns it
(define (create-button btn)
  (cond
    [(button-rounded? btn) (overlay (text (string-upcase (button-text btn)) (button-fontSize btn) "black")
                                    (ellipse (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]
    [else 
     (overlay (scale .5 (text (string-upcase (button-text btn)) (button-fontSize btn) "black"))
              (rectangle (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]))

;; button-pressed? x y -> boolean
;; Purpose: Determins if a specific button was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the button. If so returns true
(define (button-pressed? mouse-x mouse-y btn)
  (cond
    [(and (and (> mouse-x (- (posn-x (button-location btn)) (/ (button-width btn) 2)))
               (< mouse-x (+ (posn-x (button-location btn)) (/ (button-width btn) 2))))
          (and (> mouse-y (- (posn-y (button-location btn)) (/ (button-height btn) 2)))
               (< mouse-y (+ (posn-y (button-location btn)) (/ (button-height btn) 2)))))
     #t]
    [else #f]))

;; run-function: button world -> procedure
;; Purpose: Runs a buttons onClick function if the function has one. Else returns null
(define (run-function btn a-world)
  (cond
    [(procedure? (button-onClick btn)) ((button-onClick btn) a-world)]
    [else null]))