#lang racket
(require 2htdp/image 2htdp/universe)

(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 700) ;; The height of the scene
(define E-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene

(define-struct world (string)) ;; TODO add stuff to world

(define INIT-WORLD (make-world "Hello World")) ;; Create the empty world

(define (draw-world w)
  (place-image (create-gui-top) (/ WIDTH 2) 50
               (place-image (create-gui-bottom) (/ WIDTH 2) (- HEIGHT 50)
                            (place-image (rectangle 200 HEIGHT "outline" "gray") (- WIDTH 100) (/ HEIGHT 2) E-SCENE))))


;; create-gui-top: null -> image
;; Creates the top of the gui layout
(define (create-gui-top)
  (overlay/align "left" "middle"
                 (top-input-label)
                 (overlay/align "left" "middle"
                                (los-top-label)
                                (rectangle WIDTH 100 "outline" "gray"))))


;; top-input-label: null -> image
;; Purpose: Creates the top left input lable
(define (top-input-label)
  (overlay
   (text (string-upcase "Input") 24 "Black")
   (rectangle (/ WIDTH 11) 100 "outline" "red")))


;; los-top-label: null -> Image
;; Purpose: Creates the top list of sigmas lable
(define (los-top-label)
  (overlay
   (text (string-upcase "List of sigmas goes here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 90) 100 "outline" "green")))


;; create-gui-bottom: null -> image
;; Purpose: Creates the top of the gui layout
(define (create-gui-bottom)
  (overlay/align "left" "middle"
                 (rules-bottom-label)

                 (overlay/align "left" "middle"
                                (lor-bottom-label)
                                (rectangle WIDTH 100 "outline" "gray"))))


;; rules-bottom-label: null -> image
;; Purpose: Creates the left bottom label in the gui
(define (rules-bottom-label)
  (overlay
   (text (string-upcase "Rules") 24 "Black")
   (rectangle (/ WIDTH 11) 100 "outline" "red")))


;; lor-bottom-label: null -> image
;; Purpose: The label for the list of rules
(define (lor-bottom-label)
  (overlay
   (text (string-upcase "List of rules will go here") 24 "Black")
   (rectangle (- (- WIDTH (/ WIDTH 11)) 90) 100 "outline" "green")))


(big-bang
    INIT-WORLD
  (on-draw draw-world))