#lang racket

(require 2htdp/image 2htdp/universe "posn.rkt")

(define COLOR-CHANGER 2) ;; The number to change a color

(define E-SCENE (empty-scene 600 100 "yellow"))

(define TBOX (rectangle 500 50 "solid" "green"))

(struct textbox (width height color text location active))


(define BOX (textbox 500 50 (make-color 100 0 250) "" (posn 300 50) #f))

(define-struct world (input))

(define INIT-WORLD (make-world ""))

(define A-WORLD (make-world "Marco"))

;;(define (draw-world w)
;;  (overlay (text (string-upcase (world-input w)) 32 "black") TBOX E-SCENE))

(define (draw-world x)
  (draw-textbox BOX E-SCENE))

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
  (textbox (textbox-width tbox) (textbox-height tbox) (active-color (textbox-color tbox)) (textbox-text tbox) (textbox-location tbox) #true))

;; set-inactive: textbox -> textbox
;; Purpose: Sets a textbox to inactive
(define (set-inactive tbox)
  (textbox (textbox-width tbox) (textbox-height tbox) (inactive-color (textbox-color tbox)) (textbox-text tbox) (textbox-location tbox) #false))

;; active-color: color -> color
;; Purpose: given a color will shade the color so it becomes active
(define (active-color c)
  (make-color
   (/ (color-red c) COLOR-CHANGER)
   (/ (color-blue c) COLOR-CHANGER)
   (/ (color-blue c) COLOR-CHANGER)))

;; inactive-color: color -> color
;; Purpose: given a color will shade the color so it becomes inactive
(define (inactive-color c)
  (make-color
   (* (color-red c) COLOR-CHANGER)
   (* (color-blue c) COLOR-CHANGER)
   (* (color-blue c) COLOR-CHANGER)))


(define (process-key w k)
  (cond [(textbox-active BOX)
         (cond
           [(or (or (key=? k "-") (key=? k " "))
                (string<=? "a" k "z"))
            (make-world (string-append (world-input w) k))]
           [(key=? k "\b")
            (make-world (substring (world-input w)
                                   0
                                   (sub1 (string-length (world-input w)))))]
           [else w])]
        [else w]))

(define (process-mouse-event w x y me)
  (cond
    [(string=? me "button-down")
     (cond
       [(textbox-pressed? x y BOX) (begin
                                 (set! BOX (set-active BOX))
                                 (println "Textbox pressed"))]
       [else null])]
    [else null]))


(big-bang
    INIT-WORLD
  (on-draw draw-world)
  (on-key process-key)
  (on-mouse process-mouse-event))



