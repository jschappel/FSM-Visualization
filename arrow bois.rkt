#lang racket
(require 2htdp/image 2htdp/universe)

(define INNER-R 125)


;; inner-circle1: num symbol -> image
;; Purpose: draws an arrow with the given symbol above it and then rotates it by the given degreese
(define (inner-circle1 deg sym)
  (letrec
      (
       ;; arrow: none -> image
       ;; Purpose: draws a arrow
       (arrow (lambda ()
                (overlay/offset 
                 (text (symbol->string sym) 18 "red")
                 15 15
                 (beside/align "center"
                               (rectangle (- INNER-R 15) 5 "solid" "pink")
                               (rotate 270 (triangle 15 "solid" "pink"))))))

       ;; down-arrow: none -> image
       ;; Purpose: creates an upside-down arrow
       (down-arrow (lambda ()
                     (overlay/offset 
                      (rotate 180 (text (symbol->string sym) 18 "red"))
                      15 -15
                      (beside/align "center"
                                    (rectangle (- INNER-R 15) 5 "outline" "pink")
                                    (rotate 270 (triangle 15 "solid" "pink")))))))
    (cond
      ;; if if the rotate deg is > 90 and < 180, if so then use the upside-down arrow
      [(and (> deg 90) (< deg 270))
       (rotate deg (overlay/offset
                    (down-arrow)
                    -65 -8
                    (circle INNER-R "outline" "transparent")))]
      [else
       (rotate deg (overlay/offset
                    (arrow)
                    -65 8
                    (circle INNER-R "outline" "transparent")))])))

;; inner-circle2: num -> image
;; Purpose: Draws a doted line and rotates it by the given degreese
(define (inner-circle2 deg)
  (letrec
      ((dot-line (lambda ()
                   (beside
                    (line (- INNER-R 10) 0 (pen "gray" 5 "short-dash" "butt" "bevel"))
                    (circle 5 "solid" "gray")))))
    (rotate deg (overlay/align "right" "center"
                               (dot-line)
                               (circle INNER-R "outline" "transparent")))))

;; MAIN DRIVER
(define (draw-all)
  (overlay
   (circle 5 "solid" "red")
   (inner-circle1 125 'J)
   (inner-circle2 115)
   (circle INNER-R "outline" "blue")))