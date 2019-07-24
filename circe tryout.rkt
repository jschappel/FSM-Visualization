#lang racket

(require 2htdp/image 2htdp/universe)
(require math)

;;world is a list of nodes
;;length of list determines the points on the circle
;;360/length of l + l[i]
(define width 500)
(define height 500)

(define x0 (/ width 2))
(define y0 (/  height 2))

(define r (/ (- width 50) 2))
(define inner-r(- r 25))
(define the-circle (circle r "outline" "transparent"))

(struct world(los current) #:transparent)

(struct posn (x y) #:transparent)

(define e-scene (empty-scene width height "white"))


(define a-state (square 10 "solid" "black"))

(define main-scene (place-image the-circle (/ width 2) (/ height 2) e-scene))

;;world is a list of states
(define INIT-WORLD (world '(a b c d e f g h i j k) 'e))

(define the-arrow (triangle 20 "solid" "tan"))

(define (draw-world world)
  (letrec((deg-shift (/ 360 (length (world-los world))))
          (current-index (index-of (world-los world) (world-current world)))
          (tip-x (get-x (* deg-shift current-index) inner-r))
          (tip-y(get-y (* deg-shift current-index) inner-r))
          (draw-states
           (lambda (l i)
             (cond[(empty? l) main-scene]
                  [else (place-image (text (symbol->string (car l)) 25 "red")
                                     (get-x (* deg-shift i) r)
                                     (get-y (* deg-shift i) r)
                                     (draw-states (cdr l) (add1 i)))]))))
    (place-image (rotate (* deg-shift current-index) the-arrow) tip-x tip-y (add-line (draw-states (world-los world) 0) x0 y0 tip-x tip-y "black"))))


(define (get-x theta rad)
  (truncate (+ (* rad (cos (degrees->radians theta))) x0)))
                
(define (get-y theta rad)
  (truncate (+ (* rad (sin (degrees->radians theta))) y0)))
                    
