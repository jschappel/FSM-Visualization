#lang racket

(require 2htdp/image 2htdp/universe "posn.rkt")


;; ***** input.rkt *****
;; This file contains the functionality for an input field


(provide
 (struct-out textbox)
 draw-textbox
 textbox-pressed?
 add-text
 remove-text
 set-active
 set-inactive)

(define COLOR-CHANGER 2) ;; The number to change a color


;; textbox: A structure that represents a GUI Input box
;; - width: integer, the width of the textbox
;; - height: integer, the height of the textbox
;; - color: color-struct, the color of the textbox
;; - text: string, the text to be displayed
;; - location: posn-struct, the position of the textbox on the scene
;; - active: boolean, ALWAYS SET TO FALSE.
(struct textbox (width height color text location active) #:transparent)

;; draw-button: textbox scene -> scene
;; Purpose: Draws a given textbox onto the scene
(define (draw-textbox tBox scn)
  (place-image (overlay
                (text (string-upcase (textbox-text tBox)) 24 "black")
                (rectangle (textbox-width tBox) (textbox-height tBox) "solid" (textbox-color tBox)))
               (posn-x (textbox-location tBox)) (posn-y (textbox-location tBox)) scn))

;; textbox-pressed? x y -> boolean
;; Purpose: Determins if a specific textbox was pressed
;; When given an x and y corrdinate, will determine if that coordinate was inside the textbox. If so returns true
(define (textbox-pressed? mouse-x mouse-y tbox)
  (cond
    [(and (and (> mouse-x (- (posn-x (textbox-location tbox)) (/ (textbox-width tbox) 2)))
               (< mouse-x (+ (posn-x (textbox-location tbox)) (/ (textbox-width tbox) 2))))
          (and (> mouse-y (- (posn-y (textbox-location tbox)) (/ (textbox-height tbox) 2)))
               (< mouse-y (+ (posn-y (textbox-location tbox)) (/ (textbox-height tbox) 2)))))
     #t]
    [else #f]))


;; add-text: textbox string -> textbox
;; Purpose: Adds text onto an existing textbox
(define (add-text tbox msg)
  (textbox (textbox-width tbox) (textbox-height tbox) (textbox-color tbox) (string-append (textbox-text tbox) msg) (textbox-location tbox) (textbox-active tbox)))

;; remove-text: textbox int -> textbox
;; Purpose: Removes a specified amount of text from a textbox 
(define (remove-text tbox num)
  (textbox (textbox-width tbox) (textbox-height tbox) (textbox-color tbox) (substring (textbox-text tbox) 0 (- (string-length (textbox-text)) num)) (textbox-location tbox) (textbox-active tbox)))

;; set-active: textbox -> textbox
;; Purpose: Sets a textbox to active
(define (set-active tbox)
  (textbox (textbox-width tbox) (textbox-height tbox) (active-color (textbox-color tbox)) (textbox-text tbox) (textbox-location tbox) #t))

;; set-inactive: textbox -> textbox
;; Purpose: Sets a textbox to inactive
(define (set-inactive tbox)
  (textbox (textbox-width tbox) (textbox-height tbox) (inactive-color (textbox-color tbox)) (textbox-text tbox) (textbox-location tbox) #f))

;; active-color: color -> color
;; Purpose: given a color will shade the color so it becomes active
(define (active-color c)
  (make-color
   (truncate (/ (color-red c) COLOR-CHANGER))
   (truncate (/ (color-blue c) COLOR-CHANGER))
   (truncate (/ (color-blue c) COLOR-CHANGER))))

;; inactive-color: color -> color
;; Purpose: given a color will shade the color so it becomes inactive
(define (inactive-color c)
  (make-color
   (* (color-red c) COLOR-CHANGER)
   (* (color-blue c) COLOR-CHANGER)
   (* (color-blue c) COLOR-CHANGER)))