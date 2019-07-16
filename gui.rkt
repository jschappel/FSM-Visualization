#lang racket
(require 2htdp/image 2htdp/universe)


(define WIDTH 1200) ;; The width of the scene
(define HEIGHT 700) ;; The height of the scene
(define E-SCENE (empty-scene WIDTH HEIGHT "white")) ;; Create the initial scene

(define-struct world (string)) ;; TODO add stuff to world

(define INIT-WORLD (make-world "Hello World")) ;; Create the empty world

(define (draw-world w)
  (place-image (rectangle WIDTH 100 "solid" "gray") (/ WIDTH 2) 50
               (place-image (rectangle WIDTH 100 "solid" "gray") (/ WIDTH 2) (- HEIGHT 50)
                            (place-image (rectangle 200 HEIGHT "solid" "gray") (- WIDTH 100) (/ HEIGHT 2) E-SCENE)))
  )



(big-bang
    INIT-WORLD
  (on-draw draw-world))