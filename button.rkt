#lang racket
;; draw button
;; button (struct)
;; button-pressed?
(require 2htdp/image 2htdp/universe)

;; Export functions
(provide
 (struct-out button)
 (struct-out posn)
 draw-button
 button-pressed?)


;; Basic definition for posn
(struct posn (x y))

;; button: A structurre that represents a button
;; - width: integer representing the width of the button
;; - height: integer representing the height of the button
;; - text: String represting the text to go in the button
;; - type: String representing the type (solid, outline...)
;; - color: String that represents the background color of the button
;; - rounded?: Boolean representing the shape of the button. #t if rounded.
;; - location: posn that represents the location for the button
;; - onClick: 
(struct button (width height text type color rounded? location))


;; draw-button: button posn scene -> scene
;; Purpose: Draws a given button onto the scene
(define (draw-button btn scn)
  (place-image (create-button btn) (posn-x (button-location btn)) (posn-y (button-location btn)) scn))

;; create-button: button -> Image
;; create-button: creates an image that represents the button and then returns it
(define (create-button btn)
  (cond
    [(button-rounded? btn) (overlay (text (string-upcase (button-text btn)) 18 "black")
                                    (ellipse (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]
    [else 
     (overlay (scale .5 (text (string-upcase (button-text btn)) 20 "black"))
              (rectangle (button-width btn) (button-height btn) (button-type btn) (button-color btn)))]))

;; button-pressed? x y -> boolean
;; Purpose: When given an x and y corrdinate, will determine if that coordinate was inside the button. If so returns true
(define (button-pressed? mouse-x mouse-y btn)
  (cond
    [(and (and (> mouse-x (- (posn-x (button-location btn)) (/ (button-width btn) 2)))
               (< mouse-x (+ (posn-x (button-location btn)) (/ (button-width btn) 2))))
          (and (> mouse-y (- (posn-y (button-location btn)) (/ (button-height btn) 2)))
               (< mouse-y (+ (posn-y (button-location btn)) (/ (button-height btn) 2)))))
     #t]
    [else #f]))